module SecretUtility where

import System.Random
import System.Random.Shuffle
import Control.Applicative
import SecretData
import Data.List
-- | Returns True if Hitler is chancellor and there are three fascists policies down
hitlerElected :: GameState -> Bool
hitlerElected st = fascistPolicies st >= 3 && (secretIdentity . chancellor $ st) == Hitler
-- | Returns True if The vote should pass (majority Ja)
votePass :: [Vote] -> Bool
votePass vs = length (filter (Ja ==) vs) > (length vs)`div`2
-- | Returns a gamestate in which the selected policy has been passed
passPolicy :: GameState -> Policy -> GameState
passPolicy s p = s {fascistPolicies = fascistPolicies s + change Fascist,
                    liberalPolicies = liberalPolicies s + change Liberal}
               where change team = if p == Policy team then 1 else 0
-- | Get the next player for president. Next in list after president or first player if no president
findNextPresident :: GameState -> Player --Finds the next player that should be president
findNextPresident state = if (president state) `elem` (players state)
  -- If the president exists, Take the first player after you drop everyone upto and including the president 
  then head . tail $ dropWhile (/=president state) (cycle $ players state) 
  -- If the president is not in the players list, then take the first player instead
  else head . players $ state

-- | Get a player by name from a game state. Nothing indicates player not found.
getPlayer :: String -> GameState -> Maybe Player
getPlayer search state = find ((==search).name) (players state)

isPlayerIn :: String -> GameState -> Bool
isPlayerIn s gs = s `elem` (map name . players $ gs) 

getFromClient :: Client -> String -> Maybe String
getFromClient cl prefix = find (prefix `isPrefixOf`) (cliComms cl)

applyVictories :: GameState -> GameState
applyVictories gs = if fascistPolicies gs == 6
                     then gs {stopGame = Just $ Victory Fascist}
                     else if liberalPolicies gs == 5
                       then gs {stopGame = Just $ Victory Liberal}
                       else gs

freshDeck :: RandomGen gen => gen -> [Policy]
freshDeck gen = shuffle' (replicate 6 (Policy Liberal) ++ replicate 11 (Policy Fascist)) 17 gen

isVote :: String -> Bool
isVote = (||) <$> (=="Ja") <*> (=="Nein")

isPolicy :: String -> Bool
isPolicy = (||) <$> (=="Policy Liberal") <*> (=="Policy Fascist")

readElem :: (Eq a, Read a) => [a] -> String -> Bool
readElem = (. read) . flip elem 

isPolicyIn :: [Policy] -> String -> Bool
isPolicyIn ps = (&&) <$> isPolicy <*> readElem ps
--Task list
--  Give Identities
--  Select first president
bootstrapGame :: RandomGen gen => GameState -> gen -> (GameState,Bool)
bootstrapGame gs gen = if playerCount < 5
                         then (gs,False)--Not enough players, so return failure
                         else (gs' {nextPresident = findNextPresident gs'},True)
  where
    gs' = gs {players = zipWith (\a b -> b {secretIdentity = a}) shuffledIdMap (players gs),
              ready = False}
    playerCount = length . players $ gs
    shuffledIdMap = shuffle' identityMap (length identityMap) gen
    identityMap
      |playerCount < 7 = take playerCount $ [Hitler, NotHitler Fascist] ++ (repeat $ NotHitler Liberal)
      |playerCount < 9 = take playerCount $ Hitler:replicate 2 (NotHitler Fascist) ++(repeat $ NotHitler Liberal)
      |otherwise = take playerCount $ Hitler:replicate 3 (NotHitler Fascist) ++ (repeat $ NotHitler Liberal)

newGameState :: RandomGen gen => [Player] -> gen -> GameState
newGameState plas gen = GameState {players = plas,
    president = DummyPlayer,
    chancellor = DummyPlayer,
    previousGovernment = [],
    nextPresident = DummyPlayer,
    deck = freshDeck gen,
    liberalPolicies = 0,
    fascistPolicies = 0,
    electionTracker = 0,
    stopGame = Nothing,
    shardName = shardNameList !! fst (randomR (0, length shardNameList - 1) gen),
    ready = False} 

shardNameList :: [String]
shardNameList = ["Franz Anton Basch","Josef Blosche","Adolf Eichmann","Erhard Heiden","Edmund Heines","Josef Kieffer","Bruno Kitt","Fritz Knoechlein","Paul Nitsche","Friedrich Schubert","Bernhard Siebken","Wilhelm Trapp","Robert Heinrich Wagner"]
