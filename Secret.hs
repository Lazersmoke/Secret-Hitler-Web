{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import Network.WebSockets as WS
import Control.Concurrent
import SecretNetworking
import SecretUtility
import SecretData
import System.Random
import qualified Control.Monad.Parallel as Parr

-- Control flow impure code ---------------------------------------------------
main :: IO ()
main = do
  state <- newMVar $ ([],[])
  --We really don't need the thread ids; just let them die with the app
  _ <- forkIO $ mainLoop state
  _ <- forkIO $ chatLoop state
  WS.runServer "0.0.0.0" 9160 $ connHandler state

chatLoop :: MVar ServerState -> IO ()
chatLoop st = forever $ do
  state <- readMVar st
  let clients = (snd state)
  -- mapMaybe removes Nothings from the result
  modifyMVar_ st $ \mods ->
    return $ (fst mods, map (\x -> x {cliComms = filter (not . isPrefixOf "Chat|") $ cliComms x}) $ snd mods)
  let messages = mapMaybe (flip getFromClient "Chat|") clients
  mapM_ (flip tellClients clients) messages
  threadDelay 10000 
  
mainLoop :: MVar ServerState -> IO ()
mainLoop st = forever $ do 
  pruneEmptyGames st
  state <- readMVar st
  if any ready (fst state)
    then do
      let ourGame = fromJust . find ready $ fst state
      consoleLog $ "Shard " ++ shardName ourGame ++ " reports ready"
      modifyMVar_ st $ \s -> do
        let theGame = fromJust . find ready $ fst s
        return $ (theGame {ready = False} : (delete theGame $ fst s),snd s)
      --Spawn new thread to deal with each game
      _ <- forkIO $ do
        rngesus <- newStdGen
        let (bootedGame,successful) = bootstrapGame ourGame rngesus
        if not successful
          then tellEveryone "Info|You need at least 5 players to start" bootedGame
          else do
            consoleLog $ "Starting game on shard: " ++ shardName bootedGame
            givePlayerRoles bootedGame 
            stopcode <- playGame bootedGame st 
            flip tellEveryone bootedGame $ case stopcode of
              Victory Fascist -> "Info|FASCISTS WIN!!!" 
              Victory Liberal -> "Info|LIBERALS WIN!!!" 
            -- ServerState updated upon return of playGame
            modifyMVar_ st $ \s -> do
              return $ (delete bootedGame $ fst s, snd s)
            consoleLog $ "Shard " ++ shardName bootedGame ++ " terminated with " ++ show stopcode
      --Must match return type of threadDelay
      return ()
    --wait for a full second to be crazy resource light
    else threadDelay 100000 

pruneEmptyGames :: MVar ServerState -> IO ()
pruneEmptyGames state = modifyMVar_ state $ \s -> return (filter ((>0) . length . players) $ fst s, snd s)
  
givePlayerRoles :: GameState -> IO ()
givePlayerRoles gs = do
  --Tell everyone who they are
  tellEveryone ("Info|The order of play is: " ++ (intercalate " " . map name $ players gs)) gs
  forM_ (players gs) (\p -> flip tellPlayer p $ "Info|You are " ++ (keyFromIdent . secretIdentity $ p))
  if (length . players $ gs) > 6
    then forM_ (filter (hasId $ NotHitler Fascist) $ players gs) tellEverything
    else forM_ (filter ((||) <$> hasId (NotHitler Fascist) <*> hasId Hitler) $ players gs) tellEverything
  where
    keyFromIdent si = case si of
                        NotHitler Liberal -> "liberal"
                        NotHitler Fascist -> "fascist"
                        Hitler -> "hitler"
                        NoIdentity -> error "No Identity midgame rip"
    hasId s p = (==s) . secretIdentity $ p
    tellEverything p = forM_ 
      [("Info|The Liberals are ", NotHitler Liberal),
       ("Info|The Fascists are ", NotHitler Fascist),
       ("Info|Hitler is ", Hitler)]
      (\(a,b) -> flip tellPlayer p $ a ++ (intercalate " ". map name . filter (hasId b) $ players gs))

doRound :: GameState -> MVar ServerState -> IO GameState
doRound state sstate = do
  (state', electionPass) <- electGovernment state sstate
  if electionPass
    then if hitlerElected state'
      then do
        tellEveryone "Info|Hitler was elected chancellor" state'
        return state' {stopGame = Just $ Victory Fascist}
      else legislativeSession state' sstate
    else if electionTracker state' == 3
      then return $ (passPolicy state' (head . deck $ state')) {electionTracker = 0, deck = tail . deck $ state'}
      else return state' -- Election tracker modified in electGovernment

playGame :: GameState -> MVar ServerState -> IO StopCode
playGame st sstate = do
  writeState st sstate
  nst <- doRound st sstate
  writeState nst sstate
  if isJust $ stopGame nst
    then return . fromJust . stopGame $ nst
    else playGame nst sstate
electGovernment :: GameState -> MVar ServerState -> IO (GameState,Bool) -- Bool is True if a new government is in
electGovernment state sstate = do
  -- Select new president from helper function
  let newPres = nextPresident state 
  let prevGov = if (length . players $ state) < 7
                  then [chancellor state]
                  else [president state, chancellor state]
  let state' = state {president = newPres,
                      nextPresident = findNextPresident (state {president = newPres}),
                      previousGovernment = prevGov}
  -- Ask the new president for the name of the next chancellor
  newChance <- fromJust . flip getPlayer state <$> 
    askPlayerUntil 
      (\x -> ((elem DummyPlayer $ previousGovernment state') || x `notElem` (map name $ previousGovernment state')) && x /= (name . president $ state') && x `isPlayerIn` state')
      "Chancellor" 
      sstate newPres 
  tellEveryone ("Info|The proposed government is: " ++ name newPres ++ " as President and " ++ name newChance ++ " as Chancellor") state'
  -- Ask each player for their vote until it is valid, and store it in votes
  votes <- map read <$> Parr.mapM (askPlayerUntil isVote "Vote" sstate) playerList 
  tellEveryone ((++) "Info|The votes were: " $ concat $ zipWith (\a b -> "\n" ++ show a ++ " " ++ name b) votes playerList) state'
  -- check if the vote passes
  if votePass votes
    -- If it does, put in the new pres and chan and prevGov and return the new state
    then do
      tellEveryone ("Info|The vote passed!") state
      tellEveryone ("Info|The new government is: " ++ name newPres ++ " as President and " ++ name newChance ++ " as Chancellor") state
      return (state' {chancellor = newChance}, True)
    -- Otherwise, advance the election tracker and return false for failure
    else do
      tellEveryone ("Info|The vote failed! The election tracker is now in posisition " ++ (show . (+1) . electionTracker $ state) ++ "!") state
      return (state' {electionTracker = (electionTracker state) + 1}, False)
  where
    playerList = players state

-- Do a legislative session (pass a policy), then return that policy and the modified deck
legislativeSession :: GameState -> MVar ServerState -> IO GameState
legislativeSession state sstate = do
  -- Ask the president to discard a card until a valid choice is made
  discard <- read <$> 
    askPlayerUntil 
      -- The card is in the top 3 cards
      (isPolicyIn drawn)
      ("Discard|" ++ (intercalate "," $ map show drawn))
      sstate (president state)
  let cards = delete discard drawn
  -- Ask the chancellor to play a card until a valid choice is made
  playcardr <- 
    askPlayerUntil 
      (\x -> (fascistPolicies state > 4 && x=="Veto") || isPolicyIn cards x) 
      ("Play|" ++ (intercalate "," $ map show cards)) 
      sstate (chancellor state)
  playcardm <- 
    if playcardr /= "Veto"
      then return . Just $ read playcardr
      else do
        presResp <- askPlayerUntil isVote "Veto" sstate (president state)
        if presResp == "Ja"
          then return Nothing
          else Just . read <$> askPlayerUntil 
                 (isPolicyIn cards) 
                 ("Play|" ++ (concat . intersperse "," . map show $ delete discard drawn))
                 sstate (chancellor state)
  if isJust playcardm
    then do
      let playcard = fromJust playcardm 
      -- Move first three cards of the old deck to the back
      let newdeck = delete playcard $ (drop 3 $ deck state) ++ drawn
      --Pass the policy and check victories
      let newstate = applyVictories $ passPolicy (state {deck = newdeck}) playcard
      --Do the special fascist action if a fascist policy is passed
      newstate' <- case playcard of
        Policy Fascist -> do
          tellEveryone ("Info|A Fascist Policy was played!") state
          fascistAction newstate sstate
        Policy Liberal -> do 
          tellEveryone ("Info|A Liberal Policy was played!") state
          return newstate
      -- return the played card and the reshuffled deck without the playcard in it
      return newstate' 
  else do
    tellEveryone "Info|The agenda was vetoed!" state
    return $ state {deck = (drop 3 $ deck state) ++ drawn}
  where
    drawn = take 3 $ deck state

fascistAction :: GameState -> MVar ServerState -> IO GameState
fascistAction gs sstate 
  | playerCount < 7 = useActionSet [return gs, return gs, peekThree, bullet, bullet, return gs]
  | playerCount < 9 = useActionSet [return gs, investigate, specialElection, bullet, bullet, return gs]
  | otherwise = useActionSet [investigate, investigate, specialElection, bullet, bullet, return gs]
  where
   useActionSet xs = xs !! ((fascistPolicies gs) - 1)
   playerCount = length . players $ gs
   --President looks at top three cards from deck
   peekThree = do 
     tellPlayer ("Info|The top three cards are: " ++ (show . take 3 . deck $ gs)) (president gs) 
     tellEveryone ("Info|The President (" ++ (name.president$gs) ++ ") has peeked at the top three cards!") gs
     return gs
   --President looks at another player's secret identity
   investigate = do
     player <- fromJust . flip getPlayer gs <$> askPlayerUntil (`isPlayerIn` gs) "Investigate" sstate (president gs)
     tellPlayer ("Info|Player \"" ++(name player)++ "\" is " ++ (show . secretIdentity $ player)) (president gs)
     tellEveryone ("Info|The President (" ++(name.president$gs)++ ") has Investigated " ++ (name player) ++ "!") gs
     return gs
   --President chooses next canidate
   specialElection = do
     player <- fromJust . flip getPlayer gs <$> askPlayerUntil (`isPlayerIn` gs) "Special Election" sstate (president gs)
     tellEveryone ("Info|" ++ (name . president $ gs) ++ " has called a special election, with " ++ (name player) ++ " as the next presidential candidate") gs
     return $ gs {nextPresident = player}
   --President shoots (removes from game) a player
   bullet = do
     player <- fromJust . flip getPlayer gs <$> askPlayerUntil (`isPlayerIn` gs) "Kill" sstate (president gs)
     tellEveryone ("Info|" ++ (name . president $ gs) ++ " has shot and killed " ++ (name player)) gs
     if secretIdentity player == Hitler
       then do
         tellEveryone "Info|Hitler was killed!" gs
         return $ gs {stopGame = Just $ Victory Liberal, players = (delete player $ players gs)}
       else do
         tellEveryone ("Info|Rest in pieces, " ++ (name player)) gs
         tellEveryone ("Info|Confirmed not Hitler: " ++ (name player)) gs
         tellEveryone ("Disconnect|" ++ (name player)) gs
         --Return the game without the player in it. RIP player
         return $ gs {players = (delete player $ players gs)}

writeState :: GameState -> MVar ServerState -> IO ()
writeState gs ss = modifyMVar_ ss $ \s -> do
  return $ (gs : (filter ((/=players gs) . players) $ fst s), snd s)
