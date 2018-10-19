module Connect4 where

import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Ord

type Column = Int
type Row = Int

data Player = P1 | P2
    deriving (Ord, Eq, Show)

-- setup board
-- board is an ADT. Tile is the Constructor. Similar to how BSTree is an ADT and Node is the constructor
data Board  =  Tile [[Player]]  (Row, Column)
        deriving (Show)

-- make new board
-- (replicate col []) creates a list of length c 
newBoard:: (Row, Column) -> Board
newBoard (r, c) = Tile (replicate c []) (r, c)

-- checks if player won

moveWin :: Board -> Column -> Player -> Bool
moveWin bd col player 
    | checkWin (makeMove bd col player) == 1 = True
    | checkWin (makeMove bd col player) == 2 = True
    | otherwise = False

-- TODO: CAN WE SIMPLIFY ALL THESE CHECKS?? 

-- checks for a horizontal win
checkHorizontal :: [Player] -> Player -> Bool
checkHorizontal [] player = False
checkHorizontal (a:b:c:d:t) player 
    | a==b && a==c && a==d && a==player = True
    | otherwise = checkHorizontal (b:c:d:t) player
checkHorizontal (a:t) player = False

-- checks for a vertical win
checkVertical :: Board -> Player -> Bool
checkVertical (Tile [] (row, col)) player = False
checkVertical (Tile (h:t) (row, col)) player = 
    (checkHorizontal h player) || checkVertical (Tile t (row, col)) player

-- nextPlayer P1 will return P2 and vice versa
nextPlayer :: Player -> Player
nextPlayer player 
    | player == P1 = P2
    | player == P2 = P1

-- TODO: What does this do? Inputs of board, x position, y position, and the player, and ... idk what len does hmm
getItem :: Board -> Int -> Int -> Player -> Player
getItem (Tile x (row, col)) a b player 
    | b < length len = len !! b
    | otherwise = player
    where len | a < length x = x !! a | otherwise = []

-- checks for right diagonal win
checkRightDiagonal :: Board -> Player -> Int -> Int -> Bool
checkRightDiagonal x player a b = q1 == q2 && q1 == q3 && q1 == q4 && q1 == player
    where 
        p1 = nextPlayer player
        q1 = getItem x a b p1
        q2 = getItem x (a+1) (b+1) p1
        q3 = getItem x (a+2) (b+2) p1
        q4 = getItem x (a+3) (b+3) p1

checkRightDiag :: Board -> Player -> Int -> Int -> Bool
checkRightDiag x@(Tile y (r, c)) p a b | a>c-4 && b > r - 5 = False
              | a>c-4 = checkRightDiag x p 0 (b+1)
              | otherwise = checkRightDiagonal x p a b || checkRightDiag x p (a+1) b

rightCheck :: Board -> Player -> Bool
rightCheck  x p = checkRightDiag  x p 0 0


--checks for a left diagonal win
leftCheckItem :: Board -> Player -> Int -> Int -> Bool
leftCheckItem x p a b = q1 ==  q2 && q1 == q3 && q1 == q4 && q1 == p
      where
   p1 = nextPlayer p
   q1 = getItem x a b p1
   q2 = getItem x (a-1) (b+1) p1
   q3 = getItem x (a-2) (b+2) p1
   q4 = getItem x (a-3) (b+3) p1

-- leftCheckIter takes in a board, player, x position of coin, y position of coin and checks for \
-- if the bottom coin is on the bottom row, then the minimum x position is 3
-- which is why leftCheck passes a = 3, b = 0
leftCheckIter :: Board -> Player -> Int -> Int -> Bool
leftCheckIter x@(Tile y (r,c)) p a b
            -- a = 7 ; b = 2 ; r = 6 ; c = 7
            -- 7 >= 7 && 0 < (6-4)
            -- 7 >= 7 && 1 < 2
            -- 7 >= 7 && 2 < 2 .. false
            | a >= c && b < (r-4) = leftCheckIter x p 3 (b+1)
            | a >= c && b >= (r-4) = False
            | otherwise = leftCheckItem x p a b || leftCheckIter x p (a+1) b


leftCheck :: Board -> Player -> Bool
leftCheck  (Tile x (r, c))  p | c > 3 = leftCheckIter (Tile x (r,c)) p 3 0
           | otherwise = False

--checks diagonals and columns and columns of the transpose to see if Player is the winner
check :: Board -> Player -> Bool
check x p = (leftCheck x p) || (rightCheck x p) || (checkVertical x p) || (checkVertical (transboard x) p)

-- tranposes the board
transboard :: Board -> Board
transboard y@(Tile x (r,c)) = player2board ( transpose ( board2player y)) r c

-- makes array of players from board
board2player :: Board -> [[Player]]
board2player (Tile x (row, col)) = x

-- makes board from array of players, row, and col
player2board :: [[Player]] -> Row -> Column -> Board
player2board x r c = (Tile x (r,c))

-- checks if there is a winner
checkWin :: Board -> Int
checkWin bd 
    | check bd P1 = 1
    | check bd P2 = 2
    | otherwise = 0

-- checks if board full (if yes, then it's a tie)
isDraw :: Board -> Bool
isDraw (Tile [] z) = True
isDraw (Tile (x:xs) (r, c))
    | length x == r && isDraw (Tile xs (r,c)) == True = True
    | otherwise = False

-- turns board to string
showBoard :: Board -> String
showBoard b@(Tile x (r,c)) 
    | checkWin b == 0 = (whr b 1 0) ++ trailer (c-1)
    | checkWin b == 1 = (whr b 1 1) ++ trailer (c-1)
    | checkWin b == 2 = (whr b 1 2) ++ trailer (c-1)

-- generates the trailing lines for the output
trailer :: Int -> String
trailer a | a == -1 = "+\n"
    | otherwise = "+-" ++ trailer (a-1) ++ " " ++ (show a)

-- TODO: WHAT DO ALL THESE FUNCTIONS DO??

-- TODO: to change the symbols for the winning player, we need to change this function.
--creates output for each cell

-- if not a winning board
wh1 :: [Player] -> Int -> String
wh1 [] p = " |"
wh1 x 1 | head x == P1 = "X|"
  | head x == P2 = "O|"
  | otherwise = " |"
wh1 x p | p > (length x) = " |"
  | otherwise = (wh1 (tail x) (p-1))

-- if P1 is winner
xh1 :: [Player] -> Int -> String
xh1 [] p = " |"
xh1 x 1 
    | head x == P1 = "*|"
    | head x == P2 = "O|"
    | otherwise = " |"
xh1 x p 
    | p > (length x) = " |"
    | otherwise = (xh1 (tail x) (p-1))

    -- if P2 is winner
yh1 :: [Player] -> Int -> String
yh1 [] p = " |"
yh1 x 1 
    | head x == P1 = "X|"
    | head x == P2 = "*|"
    | otherwise = " |"
yh1 x p 
    | p > (length x) = " |"
    | otherwise = (yh1 (tail x) (p-1))

--seperated the cells of one row
-- if not a winning board
wh :: [[Player]] -> Int -> Int -> String

wh [x] p win
    | win == 1 = xh1 x p
    | win == 2 = yh1 x p
    | otherwise = wh1 x p
wh (x:t) p win
    | win == 1 = xh1 x p ++ wh t p win
    | win == 2 = yh1 x p ++ wh t p win
    | otherwise = wh1 x p ++ wh t p win

-- joins the rows together, original table is created from columns !
-- if not a winning board
whr :: Board -> Int -> Int -> String
whr (Tile x (r, c)) p win | p >= (c-1) = "|" ++ wh x p win ++ "\n"
         | otherwise = whr (Tile x (r, c)) (p+1) win ++ "|"  ++ wh x p win ++ "\n"

-- checks if the move is legal
--
isLegalMove:: Board -> Column -> Bool
isLegalMove (Tile [] (r, c)) z = True
isLegalMove (Tile x (r, c)) z | z >= c = False
           | otherwise = (kk < r)
              where
                kk = length k
                k  = (x !! z)

-- make one move, raise errors for illegal move
--
makeMove :: Board -> Column -> Player -> Board
makeMove y@(Tile board (r, c)) column player = Tile (a ++ b ++ c') (r, c)
          where a  = (firstHalf board column)
                b  = [((board !! column) ++ [player])]
                c' = (secondHalf board column)

--1st helper function to in makeMove, split to two part, first half
--
firstHalf:: [[Player]] -> Int -> [[Player]]
firstHalf board c = take c board

--second helper function to in makeMove, split to two part, second half
secondHalf:: [[Player]] -> Int -> [[Player]]
secondHalf board c = drop (c + 1) board

-- TODO:  simplify this
--Find out whose turn it is
turn :: Board -> Player
turn (Tile x (r, c))  | rc > yc = P2
         | otherwise = P1
           where
      rc = length (filter (==P1) u1)
      yc = length (filter (==P2) u1)
      u1 = concat x



--- PLAY GAME -----
play :: Board -> IO ()
play board | checkWin board == 1 = putStrLn "P1 wins!"
           | checkWin board == 2 = putStrLn "P2 wins!"
           -- showBoard 2 board
           | isDraw board == True = putStrLn "Draw. Game Over."
           | otherwise = do
             x <- readInteger (turn board)
             ok <- checkMove board x
             let board1 | ok  = makeMove board x (turn board)
                        | otherwise = board
             temp <- putStrLn ("\n" ++ (showBoard board1))
             play board1


--convert False to a String: Illegal Move!
--
convert :: Bool -> String
convert True = ""
convert False = "Illegal Move!\n"

--checkMove, check the move is valid or not, reture True or False
--
checkMove :: Board -> Column -> IO Bool
checkMove b c = do
                let x = isLegalMove b c
                putStr (convert x)
                return (x)

--Prompt for user imput for each turn
--
readInteger :: Player -> IO Int
readInteger player = do
                     putStr "\nPlease enter move for "
                     putStr (show player)
                     putStrLn ": "
                     x<-getLine
                     return (read x)


-- type main to start the game
main :: IO ()
-- row and col are variables to make it more easier to 
-- change later on if we want to modify the board size
row = 6
col = 7
main = play (newBoard (row,col))

--TODO:
-- reorganize
-- simplify functions
-- add our special thing
-- comment code
-- prep for demo


{-
--checks diagonals and columns and columns of the transpose to see if Player is the winner
check1 :: Board -> Player -> Bool
check1 x p = (leftCheck x p) || (rightCheck x p) || (checkVertical x p) || (checkVertical (transboard x) p)

-- iterates through all rows
iterateRows 0 _ = return
iterateRows rows columns= 
    do
        iterateColumns(columns)
        iterateRows (rows - 1)

-- iterates through all columns
iterateColumns 0 = return
iterateColumns columns = 
222222    do
        if piece == P1 || piece == P2
    do
        if piece == P1 || piece == P2
    do
        if piece == P1 || piece == P2
    do
        if piece == P1 || piece == P2
    do
        if piece == P1 || piece == P2
    do
        if piece == P1 || piece == P2
    do
        if piece == P1 || piece == P2
    do
        if piece == P1 || piece == P2
        else
        iterateColumns (columns - 1)

-}