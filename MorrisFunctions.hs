module MorrisFunctions where
import Data.List
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random

--the player can be a computer or a human
data Player = Computer | Human
  deriving (Show, Eq, Ord)
  
--the board is represented as a 3x8 matrix
--each row of the matrix is a square
-- 0 - the outer square
-- 1 - the middle square
-- 2 - the inner square (smallest)
-- the corners and the middle of the edges of each square are pointed
--with numbers from 0 - left-upper corner to 7 - left edge middle point
--clockwise
data Board = Board {squares :: [[Maybe Player]]}
  deriving (Show, Eq)

genPlayer :: Gen (Maybe Player)
genPlayer = frequency [(6, return Nothing),(9, return (Just Human)), (9, return (Just Computer))]

instance Arbitrary Player where
 arbitrary = elements [Human, Computer]
instance Arbitrary Board where
  arbitrary =
    do rows <- sequence [ sequence [ genPlayer | j <- [1..8] ] | i <- [1..3] ]
       return (Board rows)

--a function that returns an empty board for the beginning of the game
emptyBoard :: Board
emptyBoard = Board $ replicate 3 $ replicate 8 Nothing

fullBoard = Board [replicate 8 (Just Computer), replicate 8 (Just Human), 
             replicate 8 (Nothing)]
                   
--a complicated function that prints a board 
--first it prints the first three line using list comprehension to
--print the fisrt 3x3 part of the matrix
--then prints ln1, which is half of the line printed in the middle of the
--board, and it represents the last column in the Board matrix
--the second half of the middle line is represented in the matrix as the 
--column right after the 3x3 block, but it is reversed before printing
--the last part printed are the last three lines, which are represented
--in the Board matrix as 3x3 block right before the last column, but has to
--be printed up side sown, so we use list comprehension with n from 2 to 0
printBoard :: Board -> IO ()
printBoard b = 
   putStrLn $ concat [sp n ++ showP (firstThree !! n !! 0) ++
   replicate (11 - n*4) '-' ++ showP (firstThree !! n !! 1) ++
   replicate (11 - n*4) '-' ++ showP (firstThree !! n !! 2) ++ 
   ps n ++ "\n" ++ row !! n  | n<-[0..2]]
   
   ++ ln1 !! 0 ++ "---" ++ ln1 !! 1 ++ "---" ++ ln1 !! 2 ++"       "
   ++ ln2 !! 2 ++ "---" ++ ln2 !! 1 ++ "---" ++ ln2 !! 0 ++ "\n" ++
   
   concat [row !! n ++ sp n ++ showP (lastThree !! n !! 0) ++ 
   replicate (11 - n*4) '-' ++ showP (lastThree !! n !! 1) ++ 
   replicate (11 - n*4) '-' ++showP (lastThree !! n !! 2) ++
   ps n ++ "\n" | n<-[2,1,0]]				   
    where
     sp n = concat $ replicate n "|   "
     ps n = concat $ replicate n "   |"
     row = ["|           |           |\n",
            "|   |       |       |   |\n",
            "|   |   |       |   |   |\n"]
     showP p | p == Nothing       = "*"
             | p == Just Computer = "C"
             | otherwise          = "H"
     transp = transpose $ squares b
     firstThree = transpose $ take 3 $ transp
     ln1 = map showP (transp !! 7)
     ln2 = map showP (transp !! 3)
     lastThree = map reverse $ transpose [ transp !! 4, transp !! 5,
                              transp !! 6]

--the first number in the position is the square (1 -outer .. 3 -inner)
--the second number is from 1 to 8 meaning a point on a square
--point 1 is the upper corner left and it goes on clockwise
type Pos = (Int, Int)

--Function used for props to adjust the random Pos to valid positions
goodPos :: Pos -> Pos
goodPos (sq, p) = (abs sq `mod` 2, abs p `mod` 7)

availablePos :: Board -> Pos -> Bool
availablePos b (sq, p) = squares b !! sq !! p == Nothing

--property that checks if the position we updated with Nothing using
--the update function, is  actually available
prop_availablePos :: Board -> Pos -> Bool
prop_availablePos b (i,j)  = 
     availablePos (updateBoard b Nothing (i', j')) (i', j')
         where (i', j') = goodPos (i,j)


--this function checks if a move is valid. It checks if:
-- the first position contains the token of the current player
-- the second position (to move to) is empty
-- the positions are neighbours (the move is possible between them)
validMove :: Board -> Player -> Pos -> Pos -> Bool
validMove b pl (sq, p) (sq', p') = okFirstPos && okSecondPos
                                   && ((sq',p') `elem` (neighbourPos (sq,p)))
    where okFirstPos = squares b !! sq !! p == Just pl
          okSecondPos = availablePos b (sq', p')

--this property checks that after moving a token, the attempt to move it back
--is still a valid move
--I avoided to generate a random board since this is not important for this test
--The property takes a player, a position and a number n
--One player's token is placed on the empty board to assure a valid first move
--The second position p' is chosen randomly from the list of neighbours of p
--using the number n normalized to the length of the list
prop_validMove :: Player -> Pos -> Int-> Bool
prop_validMove pl pos n = validMove bAfterMove pl p' p
  where p = goodPos pos
        b'         = placeToken emptyBoard pl p
        listPos    = freeNeighb b' p
        p'         = listPos !! (abs n `mod` (length listPos - 1))
        bAfterMove = moveToken b' pl p p'

--function that changes a value in a list								 
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) ls (n,v) = take (n) ls ++ v :(drop  (n+1) ls) 
								
updateBoard :: Board -> Maybe Player -> Pos -> Board
updateBoard b mpl (sq, p) = let ls = squares b in
                       Board $ ls !!= (sq, ((ls !! sq)) !!=  (p,mpl))

--property that checks the update function					   
prop_updated :: Board -> Maybe Player -> Pos -> Bool
prop_updated b mpl (i,j) = 
      squares (updateBoard b mpl (i', j')) !! i' !! j' == mpl
         where (i', j') = goodPos (i,j)
						
placeToken :: Board -> Player -> Pos -> Board
placeToken b pl (sq, p) | availablePos b (sq, p) 
                                    = updateBoard b (Just pl) (sq, p)
                        | otherwise = b
--property that checks that a position where we placed a token 
--(if not already occupied) in not empty any more
prop_placedToken :: Board -> Player -> Pos -> Bool
prop_placedToken b pl (i,j) = 
     squares (placeToken b pl (i', j')) !! i' !! j' /= Nothing
         where (i', j') = goodPos (i,j)
prop_placedToken' :: Board -> Player -> Pos -> Bool
prop_placedToken' b pl (i,j) =
     not $ availablePos (placeToken b pl (i', j')) (i', j')
         where (i', j') = goodPos (i,j)
				
moveToken :: Board -> Player -> Pos -> Pos -> Board
moveToken b pl pos pos' | not $ validMove b pl pos pos' = b
                        | otherwise = updateBoard
                          (updateBoard b (Just pl) pos') Nothing pos

--the function creates a list of all the possible 3-in-a-row positions
--(mills)						   
millsPos :: [[Pos]]
millsPos = concat[[[(i,j), (i,(j+1)),(i,((j+2) `mod` 8))]
            |i <- [0,1,2]] ++ [[(0,j+1), (1, j+1), (2, j+1)]]
            |j <- [0,2,4,6]]
            
--the function that gives the list of 3-in-1-row positions filled
--by a player		
actualMills :: Board -> Player -> [[Pos]]
actualMills b pl = [x |x <- millsPos, map (\(i,j) -> squares b !! i !! j )
                                               x == replicate 3 (Just pl)]
											   
prop_mills :: Board -> Player -> Bool
prop_mills b pl = and [x `elem` millsPos |x <- actualMills b pl]

--function that counts the mills (3 tokens in a row) for a player
numberOfMills :: Board -> Player -> Int
numberOfMills b pl = length $ actualMills b pl

--function that returns the list of mills that are missing one token
--the head of one mill-list is the incomplete position
almostMills :: Board -> Player -> [[Pos]]
almostMills  b pl = [sort x |x <- millsPos, 
   (sort $ map (\(i,j) -> squares b !! i !! j ) x) == 
                         [Nothing, Just pl, Just pl]]

--property that checks that the almost mills returned by amostMills function
--are actually lists of 3 positions from which exactly 2 are occupied					 
prop_almostMills :: Board -> Player -> Bool
prop_almostMills b pl = and $ 
   map (\x -> (length $ catMaybes $ takeValues x) == 2) (almostMills b pl)
     where takeValues ls = map (\(i,j) -> squares b !! i !! j) ls
					
--function that returns all the neighbour positions for a given position									
neighbourPos :: Pos -> [Pos]
neighbourPos (sq, p) | even p     = lineNb
                     | sq == 0   = [(sq+1, p)] ++ lineNb
                     | sq == 1   = [(sq-1, p), (sq+1,p)] ++ lineNb
                     | otherwise = [(sq-1, p)] ++ lineNb
              where 
                lineNb = [(sq, (8+p-1) `mod` 8),(sq, (p+1) `mod` 8) ]
				
--function that searches for the closest token to a certain position
--that can be moved in
--it returns a list of positions with tokens that can be moved
neighbTokens :: Board -> Player -> Pos -> [Pos]
neighbTokens b pl pos = [(sq, p) | (sq, p) <- neighbourPos pos,
  squares b !! sq !! p == Just pl]

--function that checks which are the available neighbour positions to move in
freeNeighb :: Board -> Pos -> [Pos]
freeNeighb b pos = [(sq, p) | (sq, p) <- neighbourPos pos,
  squares b !! sq !! p == Nothing]

--function that checks if a token from a certain position is a part of a mill
partOfMill :: Board -> Player -> Pos -> Bool
partOfMill b pl pos = not $ null $ [x | x <- actualMills b pl, any (== pos) x]

--function to withdraw the opponent's piece 
takeToken :: Board -> Player -> Pos -> Maybe Board
takeToken b pl (sq, p) | pos == Nothing || pos == Just pl = Nothing
                       | partOfMill b (fromJust pos) (sq, p) = Nothing
                       | otherwise = Just $ updateBoard b Nothing (sq, p)
  where pos = squares b !! sq !! p
  
--function that returns the list of the player's positions on the board 
tokenPositions :: Board -> Player -> [Pos]
tokenPositions b pl = concat [[ (i,j)| j <- [0..7],
                                        squares b !! i !! j == Just pl]
                                     | i <- [0..2]]
freePositions :: Board -> [Pos]
freePositions b = concat [[ (i,j)| j <- [0..7], squares b !! i !! j == Nothing]
                                 | i <- [0..2]]	
--function that returns all the possible moves for a player
possibleMoves :: Board -> Player -> [(Pos, Pos)]
possibleMoves b pl = [ (i,j)| i <- tokenPositions b pl, j <- freeNeighb b i]

--function that counts the tokens for each player
countTokens :: Board -> Player -> Int
countTokens b pl = length $ tokenPositions b pl

--evaluation of moves
-- + 10 cpmputer mill
-- -9 Human mill
-- +4 almost computer moll
-- -5 akmost human mil
evalBoard :: Board -> Int
evalBoard b = (numberOfMills b Computer) * 10 - (numberOfMills b Human) * 9
   + length (almostMills b Computer) * 4 - length (almostMills b Human) * 5

--best place to place a token, according to the strategy of evaluating the
--board in the next move
bestPlace :: Board -> Pos
bestPlace b = takeRandom list
   where list =sort $ map (\x -> (evalBoard $ placeToken b Computer x, x))
                   $ freePositions b
--property that checks if the position that the computer finds best
--to place a token, it is actually a free position				   
prop_bestPlace :: Board -> Bool
prop_bestPlace b = availablePos b $ bestPlace b

--helper function that take the first and the second element from a tuple
second :: (a,b) -> b
second (x,y) = y
first :: (a,b) -> a
first (x,y) = x

--best way of moving a token
--it evaluates the board for the next move
bestMove :: Board -> Maybe (Pos, Pos)
bestMove b | null list = Nothing 
           | otherwise = Just $ second $ list !!(length list - 1)
 where
  list = sort $ map (\(x,y) -> (evalBoard $ moveToken b Computer x y,(x, y)))
   (possibleMoves b Computer)

--property that checks if the second position returned by the bestMove 
--is actually available to move on  
prop_bestMove :: Board -> Gen Prop
prop_bestMove b = (not $ bMove == Nothing) ==> 
                         (availablePos b $ second $ fromJust bMove)
  where bMove = bestMove b

--chooses the best position to capture a human token, when the computer makes
--a mill. It uses the evaluation of the board for the next move
bestCapture :: Board -> Maybe Pos
bestCapture b | null list = Nothing
              | otherwise = Just $ second $ list !!(length list - 1)
  where list = sort [(evalBoard $ fromJust $ takeToken b Computer x, x)
                  | x <- tokenPositions b Human, not $ partOfMill b Human x]

--property that checks if the best capture is not from a Human mill
prop_bestCapture :: Board -> Gen Prop
prop_bestCapture b = bCapt /= Nothing ==> 
                                not $ partOfMill b Human(fromJust bCapt)
  where bCapt = bestCapture b

--helper function that returns a random element from a sublist of positions
--for which the board are evaluated at the same level. It takes an ordered
--list and  generates a random of the last elements on the list,for which
--the board is evaluated at the same value
takeRandom :: [(Int, Pos)] -> Pos
takeRandom li = unGen (elements $ map second (dropWhile p li)) (mkStdGen 9) 155
      where p x = first x /= (first $ li !! (length li-1))