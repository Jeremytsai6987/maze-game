module GameState where

import Data.List
import Control.Exception
import qualified Data.Map as M

import Item
import Room
import Player
import Direction

type GameMap = M.Map RoomName Room

data GameState = GameState {
    message :: Maybe String,
    gmap :: GameMap,
    universe :: Universe,
    player :: Player,
    babeRuthChallenge :: Bool 
} deriving (Show)



mkMap :: [Room] -> GameMap
mkMap rooms = M.fromList [(rname room, room) | room <- rooms]

gameMap :: GameMap
gameMap = mkMap [dodgerStadium, yankeeStadium, fenwayPark, wrigleyField,
 petcoPark, americanFamilyField, oraclePark, truistPark, chaseField]

initialState :: GameState
initialState = GameState {
    message = Nothing,
    gmap = gameMap,
    universe = univ,
    player = you,
    babeRuthChallenge = False 
}

data KeyError = KeyError
  deriving Show

instance Exception KeyError

getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap rname newRoom gmap = 
    if M.member rname gmap 
    then M.insert rname newRoom gmap 
    else gmap

setMessage :: String -> GameState -> GameState
setMessage msg st = st {message = Just msg}

currentInventory :: GameState -> [ItemName]
currentInventory st = inventory (player st)

currentRoom :: GameState -> Room
currentRoom st = getRoom (location (player st)) st

nearbyObjects :: GameState -> [ItemName]
nearbyObjects st = objects (currentRoom st)

currentSalary :: GameState -> Integer
currentSalary st = sum [salary (getObject item st) |
 item <- currentInventory st]

takeItem :: ItemName -> GameState -> GameState
takeItem iname st
    | iname == ShoheiOhtani = 
        st { message =
             Just "You encounter Babe Ruth! Enter his career years to win or lose!", 
             babeRuthChallenge = True }
    | otherwise = 
        case alreadyHaveTakeCheck iname st >>=
             inRoomTakeCheck iname >>= salaryCheck iname of
            Left err -> st { message = Just err }
            Right validSt -> 
                let newPlayer = Player.addItem iname (player validSt)
                    newRoom = Room.removeItem iname (currentRoom validSt)
                    newMessage = "You take the " ++ show iname ++ "."
                in validSt { player = newPlayer
                           , gmap = setRoomMap (location newPlayer) newRoom (gmap validSt)
                           , message = Just newMessage }

dropItem :: ItemName -> GameState -> GameState
dropItem iname st = 
    case anywhereDropCheck iname st >>= inRoomDropCheck iname of
        Left err -> st { message  = Just err}
        Right validSt ->
            let newPlayer = Player.removeItem iname (player validSt)
                newRoom = Room.addItem iname (currentRoom validSt)
                newMessage = "You drop the " ++ show iname ++ "."
            in validSt { player = newPlayer
                       , gmap = setRoomMap (location newPlayer) newRoom (gmap validSt)
                       , message = Just newMessage }

type Error a = Either String a

salaryCheck :: ItemName -> GameState -> Error GameState
salaryCheck iname st
    = let item = getObject iname st
          newSalary = currentSalary st + salary item
      in if newSalary > maxSalary (player st)
          then Left "Adding this player exceeds your salary cap."
          else Right st

alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck iname st
    = if iname `elem` currentInventory st
        then Left ("You are already carrying the " ++ show iname)
        else Right st

inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck iname st
    = if iname `elem` nearbyObjects st
        then Right st
        else Left ("There is no " ++ show iname ++ " in this room.")

anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck iname st
    = if iname `elem` currentInventory st || iname `elem` objects (currentRoom st)
        then Right st
        else Left ("What do you mean, drop the \"" ++ show iname ++ "\"?")

inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck iname st
    = if iname `elem` objects (currentRoom st)
        then Left ("You aren't carrying the " ++ show iname ++ ".")
        else Right st


roomHasObjects :: GameState -> Bool
roomHasObjects st = hasObjects (currentRoom st)

destinationName :: Direction -> Room -> Maybe (RoomName, Maybe ItemName)
destinationName dir room = 
    case filter (\(d, _, _) -> d == dir) (exits room) of
        ((_, rname, maybeKey):_) -> Just (rname, maybeKey)
        [] -> Nothing

updateState :: RoomName -> String -> GameState -> GameState
updateState newRoomName message st =
    let newPlayer = (player st) { location = newRoomName }
    in st { player = newPlayer, message = Just message }

move :: Direction -> GameState -> GameState
move dir st =
    case destinationName dir (currentRoom st) of
        Just (newRoomName, maybeKey) ->
            let playerInv = inventory (player st)
            in case maybeKey of
                Nothing -> 
                    updateState newRoomName ("You go " ++ show dir ++ ".") st
                Just key ->
                    if key `elem` playerInv
                    then unlockDoor dir newRoomName key st
                    else setMessage ("The door is locked. You need a " ++ show key ++ ".") st
        Nothing -> setMessage "There is no exit in that direction." st


unlockDoor :: Direction -> RoomName -> ItemName -> GameState -> GameState
unlockDoor dir newRoomName key st =
    let updatedRoom = unlockExit dir (currentRoom st)
        newPlayer = (player st) { location = newRoomName }
        message = "You unlock the door with your " ++ show key ++ " and go " ++ show dir ++ "."
    in st { player = newPlayer
          , gmap = setRoomMap (rname updatedRoom) updatedRoom (gmap st)
          , message = Just message }

unlockExit :: Direction -> Room -> Room
unlockExit dir room =
    let updatedExits = map (\(d, r, k) -> if d == dir then (d, r, Nothing) else (d, r, k)) (exits room)
    in room { exits = updatedExits }

haveWonGame :: GameState -> Bool
haveWonGame st = hasThreeSameTeam (player st) (universe st)

hasOhtani :: Player -> Bool
hasOhtani player = ShoheiOhtani `elem` inventory player

hasThreeSameTeam :: Player -> Universe -> Bool
hasThreeSameTeam player univ =
    let teams = getPlayerTeams player univ
        teamCounts = countOccurrences teams
    in any (\(_, count) -> count >= 3) teamCounts



getPlayerTeams :: Player -> Universe -> [TeamName]
getPlayerTeams player univ = 
    map (\iname -> team (getObjectUniv iname univ)) (inventory player)

countOccurrences :: (Ord a) => [a] -> [(a, Int)]
countOccurrences xs = M.toList $ M.fromListWith (+) [(x, 1) | x <- xs]



