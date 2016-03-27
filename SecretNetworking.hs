{-# LANGUAGE OverloadedStrings #-}
module SecretNetworking where

import SecretData
import Data.List
import Data.Maybe
import Network.WebSockets as WS
import System.Random
import qualified Data.Text as T
import Control.Exception
import Control.Monad
import Control.Concurrent
import qualified Control.Monad.Parallel as Parr
import SecretUtility
connHandler :: MVar ServerState -> WS.ServerApp
connHandler state connection = do
  debugLog "Got connection"
  -- Accept Connection
  accepted <- WS.acceptRequest connection
  -- Fork ping thread to keep connection alive
  WS.forkPingThread accepted 30
  -- Wait for username message
  msg <- WS.receiveData accepted
  -- Acknowledge connection
  WS.sendTextData accepted $ T.pack ("Welcome, " ++ (T.unpack $ T.drop 9 msg))
  debugLogClient (T.unpack $ T.drop 9 msg) (T.unpack msg)
  --Send list of possible shards
  shardingThread <- forkIO . forever $ do
    shardList <- shardListing state
    WS.sendTextData accepted $ T.pack shardList
    threadDelay 1000000

  shardChoice <- WS.receiveData accepted
  killThread shardingThread
  st <- readMVar state
  debugLogClient (T.unpack $ T.drop 9 msg) $ T.unpack shardChoice
  case msg of
      -- Wrong Prefix
    _ | not $ prefix `T.isPrefixOf` msg -> do
          debugLogClient (cliName client) "Sending Bad" 
          WS.sendTextData accepted ("Wrong message prefix, disconnecting" :: T.Text) 
          disconnect
      | elem (cliName client) $ bannedNames ++ (map cliName $ snd st) -> do
          debugLog $ "Client attempted to join with bad username: " ++ (cliName client)
          WS.sendTextData accepted ("Bad username" :: T.Text)
          disconnect
      -- Make new shard
      | shard == "New" -> do
        flip finally disconnect $ do
          -- Generate a fresh shard for this player, with only them in it
          gen <- newStdGen
          let newshard = newGameState [Player {secretIdentity = NoIdentity, name = cliName client, conn = cliConn client}] gen
          debugLogClient (cliName client) $ "Spawning new shard " ++ shardName newshard 
          modifyMVar_ state $ \s -> do
            --Don't need to broadcast join into new shard
            WS.sendTextData accepted (T.pack $ "Join|" ++ cliName client)
            return $ (newshard : fst s, client : snd s)
          WS.sendTextData accepted $ T.pack ("Connected to: " ++ shardName newshard)
          waitForGame (cliConn client) state client
      -- Invalid shard
      | shard `notElem` map shardName (fst st) -> do
          debugLogClient (cliName client) "Sending invalid shard" 
          WS.sendTextData accepted ("Invalid shard, disconnecting" :: T.Text)
          disconnect
      -- All is peachy
      | otherwise -> do 
        flip finally disconnect $ do
          debugLogClient (cliName client) $ "Connecting to shard " ++ shard
          WS.sendTextData accepted $ T.pack ("Connected to: " ++ shard)
          modifyMVar_ state $ \s -> do
            -- On the clock now
            let oldstate = head $ filter ((==shard) . shardName) (fst s)
            let newstate = oldstate {players = Player{secretIdentity = NoIdentity, conn = cliConn client, name = cliName client}:players oldstate}
            --Broadcast our connection to everyone else
            mapM_ (flip WS.sendTextData (T.pack $ "Join|" ++ cliName client) . conn) $ players oldstate
            --Retrocast everyone elses connection to our connection
            mapM_ (WS.sendTextData accepted . T.pack . ("Join|"++) . name) . reverse . players $ newstate
            return $ (newstate : (delete oldstate $ fst s), client : snd s)
          waitForGame (cliConn client) state client
 
      where
        bannedNames = ["Ja","Nein","Hitler","NotHitler","Join","Ask"]
        shard = T.unpack shardChoice
        prefix = T.pack "Hi! I am "
        client = Client {cliComms = [], cliName = T.unpack $ T.drop (T.length prefix) msg, cliConn = accepted}
        disconnect = do
          -- Remove client and return new state
          debugLog $ "Disconnecting client: " ++ cliName client
          nst <- readMVar state
          mapM_ (flip WS.sendTextData (T.pack $ "Disconnect|" ++ cliName client) . cliConn) $ snd nst
          modifyMVar_ state $ \s ->
            return $ (map (\x -> x {players = filter ((/= cliName client) . name) (players x)}) $ fst s, filter ((/= cliName client) . cliName) (snd s))

waitForGame :: WS.Connection -> MVar ServerState -> Client -> IO ()
waitForGame connection state client = forever $ do
  msg <- WS.receiveData connection
  debugLogClient (cliName client) $ T.unpack msg
  if T.unpack msg == "debug"
    then do
      st <- readMVar state
      debugLog $ show st
    else return ()
  if T.unpack msg == "ready"
    then modifyMVar_ state $ \s -> do
      let ourGame = find (elem (cliName client) . map name . players) (fst s)
      if isJust ourGame
        then return $ ((fromJust ourGame) {ready = True} : (delete (fromJust ourGame) $ fst s),snd s)
        else return s
    else return ()
  modifyMVar_ state $ \s -> do
    let ourClient = head $ filter ((==cliName client) . cliName) (snd s)
    let woOurClient = filter ((/=cliName client) . cliName) (snd s)
    return $ (fst s, (ourClient {cliComms = T.unpack msg : (cliComms ourClient)}:woOurClient))

shardListing :: MVar ServerState -> IO String
shardListing ss = do
  st <- readMVar ss
  return $ "Shards: " ++ (concat . intersperse " " $ map (\x -> "\n" ++ shardName x ++ ": " ++ (show . length . players $ x)) (fst st))

tellEveryone :: String -> GameState -> IO ()
tellEveryone s state = Parr.mapM_ (tellPlayer s) (players state)

tellClients :: String -> [Client] -> IO ()
tellClients s cls = Parr.mapM_ (tellClient s) cls

tellClient :: String -> Client -> IO ()
tellClient s cl = WS.sendTextData (cliConn cl) . T.pack $ s

tellPlayer :: String -> Player -> IO ()
tellPlayer s p = WS.sendTextData (conn p) . T.pack $ s

askPlayerUntil :: (String -> Bool) -> String -> MVar ServerState -> Player -> IO String
askPlayerUntil cond s sstate p = do
  tellPlayer ("Ask|" ++ s) p
  loop s p sstate
  where
    check p' sstate' = do
      outState <- readMVar sstate'
      let ourClient = fromJust $ find ((== name p') . cliName) (snd outState)
      if isJust $ getFromClient ourClient "Resp|"
        then do
          modifyMVar_ sstate $ \mods ->
            return $ (fst mods, ourClient {cliComms = delete (fromJust $ getFromClient ourClient "Resp|") $ cliComms ourClient}: (delete ourClient $ snd mods))
          return $ getFromClient ourClient "Resp|"
        else return $ getFromClient ourClient "Resp|"
    loop s' p'' state'' = do
      --Wait so we don't get too spammy
      threadDelay 10000
      checked <- check p'' state''
      if isJust checked && cond (drop 5 . fromJust $ checked)
        then do
          return . drop 5 $ fromJust checked
        else do
          when (isJust checked) $ tellPlayer ("Ask|" ++ s') p''
          loop s' p'' state''

askPlayer :: String -> MVar ServerState -> Player -> IO String
askPlayer = askPlayerUntil (\_ -> True)


