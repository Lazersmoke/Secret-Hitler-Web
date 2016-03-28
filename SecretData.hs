module SecretData where

import Network.WebSockets as WS
import Control.Monad
data StopCode = Victory Team deriving (Show,Eq,Read)
type ServerState = ([GameState],[Client])
data Client = Client {cliName :: String, cliConn :: WS.Connection, cliComms :: [String]}
data Identity = Hitler | NotHitler Team | NoIdentity deriving (Show,Eq,Read)
data Player = Player {secretIdentity :: Identity, name :: String, conn :: WS.Connection, playerReady :: Bool} | DummyPlayer
data Policy = Policy Team deriving (Show,Eq,Read)
data Vote = Ja | Nein deriving (Show,Eq,Read)
data Team = Liberal | Fascist deriving (Show,Eq,Read)

instance Show Client where
  show cli = cliName cli ++ " Que:" ++ (concat . cliComms $ cli)

instance Eq Client where
  (==) a b = cliName a == cliName b

instance Show Player where
  show DummyPlayer = "<Dummy Player>"
  show pla = "Player: " ++ name pla ++ "/" ++ (show . secretIdentity $ pla) ++ (show . playerReady $ pla)

instance Eq Player where
  (==) DummyPlayer DummyPlayer = True
  (==) DummyPlayer _ = False
  (==) _ DummyPlayer = False
  (==) a b = name a == name b
{-
Communication model:
  On Client:
    - send handshake with connection info and player name
    - server sends session list and confirm
    - client chooses session, then blocks until a message is sent
  On Server:
    - Wait for client handshakes
    - maintain session list
    - run game when all players ready
-}
data GameState = GameState {players :: [Player],
  president :: Player,
  chancellor :: Player,
  previousGovernment :: [Player],
  nextPresident :: Player,
  deck :: [Policy],
  liberalPolicies :: Int,
  fascistPolicies :: Int,
  electionTracker :: Int,
  stopGame :: Maybe StopCode,
  shardName :: String,
  ready :: Bool} deriving (Eq,Show)

debug :: Bool
debug = True

debugLog :: String -> IO ()
debugLog s = when debug $ putStrLn s

debugLogClient :: String -> String -> IO ()
debugLogClient clientName s = when debug $ putStrLn $ "<" ++ clientName ++ "> " ++ s

consoleLog :: String -> IO ()
consoleLog = putStrLn . ("[Server]"++)
