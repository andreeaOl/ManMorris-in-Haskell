module MorrisGame where
import Data.List
import Data.Maybe
import Data.Char
import MorrisFunctions

type State = (Board, Int, Int, Player)
getBoard :: State -> Board
getBoard (b, _, _, _) = b
getHumanTokens :: State -> Int
getHumanTokens (_, h, _, _) = h
getComputerTokens :: State -> Int
getComputerTokens (_, _, c, _) = c
getPlayer :: State -> Player
getPlayer (_, _, _, pl) = pl

initialState :: State
initialState = (emptyBoard, 9, 9, Human)

isMovePhase :: State -> Bool
isMovePhase s = getHumanTokens s <= 0 && getComputerTokens s <= 0

--function that returns the winner
winner :: State -> Maybe Player
winner s | not $ isMovePhase s = Nothing
         | loses Computer = Just Human
         | loses Human = Just Computer
         | otherwise = Nothing 
    where loses pl = countTokens (getBoard s) pl < 3 || 
                     (null $ possibleMoves (getBoard s) pl)

startGame :: IO ()
startGame = do
   printBoard (getBoard initialState)
   gameLoop initialState
   
gameLoop :: State -> IO ()
gameLoop s= do
   let win = winner s
   if (win == Just Human) then
         putStrLn "Congratulations! You won!"   
       else if (win == Just Computer) then
         putStrLn "You lost!"
       else if (isMovePhase s) then
          playMove s
       else
          playPlace s

playPlace :: State -> IO ()
playPlace s = 
 if ((getPlayer s) == Human)  then do -- player's turn
            putStrLn "\nYOUR TURN"
            putStrLn ("you have " ++ (show (getHumanTokens s))
                ++ " piece(s) left")
            putStr "pick a position for your next token: "
            pos <- playerChoice (getBoard s)
            let newBoard = placeToken (getBoard s) Human pos
            putStrLn "\nthe board after your move:"
            printBoard newBoard
            if (partOfMill newBoard Human pos) then do
                    putStrLn "\nYOU MADE A MILL!"
                    newerBoard <- humanCapture newBoard
                    gameLoop (newerBoard, getHumanTokens s -1,
					    getComputerTokens s, Computer)
                else
                    -- no capture, continue the game
                    gameLoop (newBoard, getHumanTokens s -1, getComputerTokens s, Computer)
        else do -- computer's turn
            putStrLn "\nMY TURN"
            let pos = bestPlace $ getBoard s
            putStrLn ("I am adding a piece to position " ++ (show pos)) 
            let newBoard = placeToken (getBoard s) Computer pos
            putStrLn "the board after my move:"
            printBoard newBoard
            if (partOfMill newBoard Computer pos) then do
                let bestCap = fromJust $ bestCapture newBoard
                let newerBoard = fromJust $ takeToken newBoard Computer (bestCap)
                putStrLn ("I made a mill. I took your token from: " ++ (show bestCap))
                printBoard newerBoard
                gameLoop (newerBoard, getHumanTokens s, getComputerTokens s -1 , Human)
               else
                 --no capture, continue the game
                gameLoop (newBoard, getHumanTokens s, getComputerTokens s -1 , Human)
playMove :: State -> IO ()
playMove s = 
    if ((getPlayer s) == Human) then do -- human player's turn
            putStrLn "\nYOUR TURN"
            putStr "pick a position to move the token: "
            fromPos <- choosePosition
            putStrLn "where do you want to move it?"
            toPos <- choosePosition
            let board = moveToken (getBoard s) Human fromPos toPos
            -- !!!! check for nothing results
            putStrLn "the board after your move:"
            printBoard board
            if (partOfMill board Human toPos) then do
                    putStrLn "\nYOU MADE A MILL!"
                    newBoard <- humanCapture board
                    gameLoop (newBoard, getHumanTokens s,
					    getComputerTokens s, Computer)
                else
                    gameLoop (board, getHumanTokens s,
					    getComputerTokens s, Computer)
        else do -- computer's turn
            putStrLn "\nMY TURN"
            let (fromPos, toPos) = fromJust $ bestMove $ getBoard s
            putStrLn ("I am moving from " ++ (show fromPos) ++ " to " 
                      ++ (show toPos))
            let board = moveToken (getBoard s) Computer fromPos toPos
            putStrLn ("the board after my turn:")
            printBoard board
            if (partOfMill board Computer toPos) then do
                    let bestCap = fromJust $ bestCapture board
                    let newBoard = fromJust $ takeToken board Computer (bestCap)
                    putStrLn ("I made a mill. I took your token from: " ++ (show bestCap))
                    printBoard newBoard
                    gameLoop (newBoard, getHumanTokens s,
					    getComputerTokens s, Human)
                else
                    gameLoop (board, getHumanTokens s, getComputerTokens s, Human)
					
playerChoice :: Board -> IO (Int, Int)
playerChoice b = do
    input <- getLine
    if (not $ correctInput input) then do
            putStrLn "the input must be (square number, pos number)"
            choice <- (playerChoice b)
            return choice
        else do
            let (sq, p) = (read input) :: (Int, Int)
            if (sq < 0 || sq > 2 || p < 0 || p > 7) then do
                    putStrLn "square is in [1..3] and position in [1..8]"
                    choice <- (playerChoice b)
                    return choice
                else if (not $ availablePos b (sq, p)) then do
                    putStrLn "illegal choice: that position is already taken"
                    choice <- (playerChoice b)
                    return choice
                else do
                    return (sq, p)

correctInput :: String -> Bool
correctInput s = s !! 0 == '(' && isDigit (s !! 1) && s !! 2 ==','
                 && isDigit (s !! 3) && s !! 4 == ')' 
humanCapture :: Board -> IO Board
humanCapture b = do 
    putStrLn "pick a position to capture"
    input <- getLine
    let (sq, p) = (read input) :: (Int, Int)
    let newBoard = takeToken b Human (sq, p)
    if (newBoard == Nothing) then do
          putStrLn "Wrong choice!"
          humanCapture b
    else do
      putStrLn "the board after your capture:"
      printBoard $ fromJust newBoard
      return $ fromJust newBoard

choosePosition :: IO (Int, Int)
choosePosition = do
    input <- getLine
    if (not (correctInput input)) then do
            putStrLn "invalid format!"
            choice <- choosePosition
            return choice
        else do
            let position = (read input) :: (Int,Int)
            return position