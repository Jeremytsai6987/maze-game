module Player where
import Item
import Room
import Data.List
data Player = Player {
    inventory :: [ItemName],
    maxSalary :: Integer,
    location :: RoomName
} deriving (Show, Eq)


addItem :: ItemName -> Player -> Player
addItem item player = player {inventory = item : inventory player}

removeItem :: ItemName -> Player -> Player
removeItem item player = player {inventory = delete item (inventory player)}

newLocation :: RoomName -> Player -> Player
newLocation room player = player {location = room}

isCarryingAnything :: Player -> Bool
isCarryingAnything player = not (null (inventory player))

you :: Player
you = Player [] 100 DodgerStadium
