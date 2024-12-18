module Example where

import Data.List
import System.Random
import qualified Data.Map as M

import Item
import Direction
import Room
import Player
import GameState

class Example a where
    example :: IO a

choose :: [a] -> IO a
choose lst = do
    index <- randomRIO (0, length lst - 1)
    return $ lst !! index

exampleList :: IO a -> IO Int -> IO [a]
exampleList ioA ioInt = do
    n <- ioInt
    sequence $ replicate n ioA

instance Example Item where
    example = do
        name <- choose itemNames
        salary <- randomRIO (1, 40) -- 隨機薪水
        team <- choose [Dodgers, Yankees, RedSox, Cubs, Padres, Brewers,
         Giants, Braves, DBacks]
        return Item { iname = name, salary = salary, team = team }

instance Example Direction where
    example = do 
        choose [N, S, E, W]

instance Example Room where
    example = do
        rname <- choose roomNames
        let desc = "You are in a " ++ show rname ++ ". It is a randomly-generated room."
        exits <- exampleList ( do
            dir <- example :: IO Direction
            roomName <- choose roomNames
            key <- choose [Nothing, Just FenwayTicket, Just WrigleyKey] 
            return (dir, roomName, key)
            ) (randomRIO (2, 4))
        objects <- exampleList (choose itemNames) (randomRIO (2, 5))
        return Room { rname = rname, desc = desc, exits = exits, objects = objects }


instance Example Player where
    example = do
        inventorySize <- randomRIO (0, 5)
        inventory <- fmap nub $ exampleList (choose itemNames) (return inventorySize)
        location <- choose roomNames
        let maxSalary = 100 
        return Player { inventory = inventory, location = location, maxSalary = maxSalary }


instance Example GameState where
    example = do
        message <- choose [Just "One possible message.",
         Just "Yet another possible message.", Nothing]
        rooms <- exampleList (example :: IO Room) (randomRIO (3, 5)) 
        let gmap = M.fromList [(rname room, room) | room <- rooms]
        universeItems <- exampleList (example :: IO Item) (randomRIO (10, 15)) 
        let universe = M.fromList [(iname item, item) | item <- universeItems]
        player <- example :: IO Player
        return GameState { message = message, gmap = gmap,
         universe = universe, player = player, babeRuthChallenge = False }


exitExample :: IO Exit
exitExample = do
    dir <- example :: IO Direction
    roomName <- choose roomNames
    key <- choose [Nothing, Just FenwayTicket, Just WrigleyKey] 
    return (dir, roomName, key)
