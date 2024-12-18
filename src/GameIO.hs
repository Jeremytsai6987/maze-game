module GameIO where
import Control.Monad.State
import System.Exit
import System.IO

import GameState
import Player
import Command
import Room
import Item
import Control.Monad (when)


type GameIO a = StateT GameState IO a

effectChange :: (GameState -> GameState) -> GameIO ()
effectChange = modify

printMessage :: GameIO ()
printMessage = do
    currentState <- get
    case message currentState of
        Nothing -> pure ()
        Just msg -> do
            liftIO $ putStrLn msg
            put $ currentState { message = Nothing }

prompt :: GameIO ()
prompt = liftIO $ do
    putStr "-> "
    hFlush stdout

printDescription :: GameIO ()
printDescription = do
    currentState <- get
    let getcurrentRoom = currentRoom currentState
    liftIO $ putStrLn (desc getcurrentRoom)

printObjects :: GameIO ()
printObjects = do
    currentState <- get
    let getcurrentRoom = currentRoom currentState
    let objectsInRoom = objects getcurrentRoom
    if null objectsInRoom
        then return ()
        else liftIO $ do
            putStrLn "You see the following objects:"
            mapM_ (putStrLn . show) objectsInRoom

printExits :: GameIO ()
printExits = do
    currentState <- get
    let getcurrentRoom = currentRoom currentState
    let exitsInRoom = map (\(dir, _, _) -> dir) (exits getcurrentRoom) 
    liftIO $ do
        putStrLn "There are exits in the following directions:"
        mapM_ (putStrLn . show) exitsInRoom



printInventory :: GameIO ()
printInventory = do
    currentState <- get
    let playerInventory = inventory (player currentState)
    if null playerInventory
        then liftIO $ putStrLn "You aren't carrying anything."
        else liftIO $ do
            putStrLn "You are carrying the following objects:"
            mapM_ (putStrLn . show) playerInventory

printTeam :: GameIO ()
printTeam = do
    currentState <- get
    let items = inventory (player currentState)
        teams = map (\iname -> team (getObject iname currentState)) items
        teamDistribution = countOccurrences teams
    if null teamDistribution
        then liftIO $ putStrLn "You are not carrying any players."
        else do
            liftIO $ putStrLn "Your team composition:"
            mapM_ (\(team, count) -> liftIO $ putStrLn $ show team ++ ": " ++
             show count ++ " player(s)") teamDistribution
printSalary :: GameIO ()
printSalary = do
    currentState <- get
    let totalSalary = sum [salary (getObject item currentState) | item <- inventory (player currentState)]
    liftIO $ putStrLn $ "Your total salary is: " ++ show totalSalary


actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO () 
actionOverList action [] = liftIO $ putStrLn "There are no items to process."
actionOverList action items = mapM_ (\item -> do
    modify (action item)
    printMessage) items

finishGame :: GameIO ()
finishGame = do
    currentState <- get
    let playerInventory = inventory (player currentState)
        playerTeams = map (\iname -> team (getObject iname currentState)) playerInventory
        teamCounts = countOccurrences playerTeams
        sameTeamWin = any (\(_, count) -> count >= 3) teamCounts
        babeRuthDefeated = babeRuthChallenge currentState
    
    if sameTeamWin
        then do
            liftIO $ putStrLn "You have assembled a championship-worthy team!"
            liftIO $ putStrLn "Congratulations! Your team wins the 2024 World Series!"
            liftIO exitSuccess
        else if babeRuthDefeated
            then do
                liftIO $ putStrLn "You have proven yourself worthy!"
                liftIO $ putStrLn "Congratulations! You have defeated Babe Ruth and won the game!"
                liftIO exitSuccess
            else return ()




exit :: GameIO ()
exit = do
    liftIO $ putStrLn "Goodbye!"
    liftIO exitSuccess

checkGameOver :: GameIO ()
checkGameOver = do
    currentState <- get
    if haveWonGame currentState
        then finishGame
        else return ()

syntaxError :: GameIO ()
syntaxError = liftIO $ putStrLn "I don't understand that."

opening :: GameIO ()
opening = liftIO $ do
    putStrLn "Welcome to Functional MLB Game!"
    putStrLn "Your goal is to assemble the ultimate team under the salary cap."
    putStrLn "You can win by either collecting three players from the same team or discovering a legendary player!"



performCommand :: Command -> GameIO ()
performCommand cmd = case cmd of
    Look -> do
        printDescription
        printObjects
        printExits
    Move dir -> do
        effectChange (move dir)
        printMessage
    Inventory -> printInventory
    Take items -> actionOverList takeItem items
    Drop items -> actionOverList dropItem items
    Salary -> printSalary
    Team -> printTeam
    Exit -> exit


performConjunction :: Conjunction -> GameIO ()
performConjunction = mapM_ performCommand

parseConjunction :: String -> GameIO ()
parseConjunction input = 
    case parseInput input of
        Just commands -> performConjunction commands
        Nothing       -> syntaxError

repl :: GameIO ()
repl = do
    currentState <- get
    if babeRuthChallenge currentState
        then processBabeRuthChallenge
        else do
            prompt               
            input <- liftIO getLine 
            case parseInput input of 
                Just commands -> performConjunction commands 
                Nothing       -> syntaxError                
            checkGameOver 
            repl

processBabeRuthChallenge :: GameIO ()
processBabeRuthChallenge = do
    liftIO $ putStrLn "Enter Babe Ruth's career years to win (????-????):"
    input <- liftIO getLine
    if input == "1914-1935"
        then finishGame 
        else do
            liftIO $ putStrLn "Incorrect! Babe Ruth strikes you out. You lose!"
            liftIO exitSuccess 

triggerBabeRuthChallenge :: GameIO ()
triggerBabeRuthChallenge = do
    liftIO $ putStrLn "You have encountered Shohei Ohtani!"
    liftIO $ putStrLn "To win the game, you must correctly input the career years of Babe Ruth (e.g., 1914-1935)."
    liftIO $ putStrLn "If you fail, you will lose the game!"
    liftIO $ putStr "Enter Babe Ruth's career years: "
    liftIO $ hFlush stdout
    input <- liftIO getLine
    if input == "1914-1935"
        then finishGame
        else loseGame

loseGame :: GameIO ()
loseGame = do
    liftIO $ putStrLn "You failed the Babe Ruth challenge."
    liftIO $ putStrLn "Game over. Better luck next time!"
    liftIO exitSuccess

checkOhtani :: GameState -> GameIO ()
checkOhtani st = when (hasOhtani (player st)) triggerBabeRuthChallenge



