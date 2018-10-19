module Connect4 where

import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Ord

type Row = Int
type Column = Int

data Player = P1 | P2
    deriving (Ord, Eq, Show)

-- setup board
-- board is an ADT. Tile is the Constructor. Similar to how BSTree is an ADT and Node is the constructor
-- got idea of using a constructor from stackoverflow
data Board  =  Tile [[Player]]  (Row, Column)
        deriving (Show)

-- make new board
-- (replicate col []) creates a list of length c 
newBoard:: (Row, Column) -> Board
newBoard (r, c) = Tile (replicate c []) (r, c)

showPlayer :: Player -> [Char]
showPlayer P1 = "P1"
showPlayer P2 = "P2"

-- nextPlayer P1 will return P2 and vice versa
nextPlayer :: Player -> Player
nextPlayer player 
    | player == P1 = P2
    | player == P2 = P1

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
checkVertical (Tile (h:t) (row, col)) player
    | (checkHorizontal h player) = True
    | otherwise = checkVertical (Tile t (row, col)) player

-- Inputs of board, x position, y position, and the player, and
-- returns a tile piece
getTile :: Board -> Int -> Int -> Player -> Player
getTile (Tile bd (row, col)) x y player 
    | y < length len = len !! y
    | otherwise = player
    where len | x < length bd = bd !! x | otherwise = []

-- checks for right diagonal win
checkRightCoin :: Board -> Player -> Int -> Int -> Bool
checkRightCoin bd player x y = t1 == player && t2 == player && t3 == player && t4 == player 
    where 
        player2 = nextPlayer player
        t1 = getTile bd x y player
        t2 = getTile bd (x+1) (y+1) player2
        t3 = getTile bd (x+2) (y+2) player2
        t4 = getTile bd (x+3) (y+3) player2

checkRightDiagIter :: Board -> Player -> Int -> Int -> Bool
checkRightDiagIter x@(Tile y (r, c)) p a b | a > c-4 && b > r - 5 = False
              | a>c-4 = checkRightDiagIter x p 0 (b+1)
              | otherwise = checkRightCoin x p a b || checkRightDiagIter x p (a+1) b

-- checks for a right diagonal win (/)
checkRightDiag :: Board -> Player -> Bool
checkRightDiag  bd p = checkRightDiagIter  bd p 0 0

--checks for a left diagonal win (\)
checkLeftCoin :: Board -> Player -> Int -> Int -> Bool
checkLeftCoin bd player x y = 
    t1 == player && t2 == player && t3 == player && t4 == player
    where
        player2 = nextPlayer player
        t1 = getTile bd x y player2
        t2 = getTile bd (x-1) (y+1) player2
        t3 = getTile bd (x-2) (y+2) player2
        t4 = getTile bd (x-3) (y+3) player2

-- checkLeftDiagIter takes in a board, player, x position of coin, y position of coin and checks for \
-- if the bottom coin is on the bottom row, then the minimum x position is 3
-- which is why checkLeftDiag passes a = 3, b = 0
checkLeftDiagIter :: Board -> Player -> Int -> Int -> Bool
checkLeftDiagIter bd@(Tile b (r,c)) player x y 
            | x >= c && y < (r-4) = checkLeftDiagIter bd player 3 (y+1)
            | x >= c && y >= (r-4) = False
            | otherwise = checkLeftCoin bd player x y || checkLeftDiagIter bd player (x+1) y

--checks (goes into checkLeftDiagIter) if columns is greater than 3, as you need at least 4 columns for a diagonal connect
-- and with our standard board, we have 7 columns, we will check for diagonals starting from the case coin in midle bottom placement
checkLeftDiag :: Board -> Player -> Bool
checkLeftDiag  (Tile x (r, c))  p | c > 3 = checkLeftDiagIter (Tile x (r,c)) p 3 0
           | otherwise = False

-- tranposes the board, switches columns into rows, and vice versa
transboard :: Board -> Board
transboard y@(Tile x (r,c)) = player2board ( transpose ( board2player y)) r c

-- makes array of players from board
board2player :: Board -> [[Player]]
board2player (Tile x (row, col)) = x

-- makes board from array of players, row, and col
player2board :: [[Player]] -> Row -> Column -> Board
player2board x r c = (Tile x (r,c))

--checks diagonals, horizontal, and vertical wins. use transpose of board to flip it
checkWinIter :: Board -> Player -> Bool
checkWinIter x p = (checkLeftDiag x p) || (checkRightDiag x p) || (checkVertical x p) || (checkVertical (transboard x) p)

-- checks if there is a winner
checkWin :: Board -> Int
checkWin bd 
    | checkWinIter bd P1 = 1
    | checkWinIter bd P2 = 2
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
    | checkWin b == 0 = (printBoard b 1 0) ++ printBottom (c-1)
    | checkWin b == 1 = (printBoard b 1 1) ++ printBottom (c-1)
    | checkWin b == 2 = (printBoard b 1 2) ++ printBottom (c-1)

-- generates the bottom lines of the board
printBottom :: Int -> String
printBottom a 
    | a == -1 = "+\n"
    | otherwise = "+-" ++ printBottom (a-1) ++ " " ++ (show a)


-- TODO: to change the symbols for the winning player, we need to change this function.
--creates output for each cell

-- if not a winning board
printNoWinBoard :: [Player] -> Int -> String
printNoWinBoard [] p = " |"
printNoWinBoard x 1 | head x == P1 = "X|"
  | head x == P2 = "O|"
  | otherwise = " |"
printNoWinBoard x p | p > (length x) = " |"
  | otherwise = (printNoWinBoard (tail x) (p-1))

-- if P1 is winner
printWinBoard1 :: [Player] -> Int -> String
printWinBoard1 [] p = " |"
printWinBoard1 x 1 
    | head x == P1 = "*|"
    | head x == P2 = "O|"
    | otherwise = " |"
printWinBoard1 x p 
    | p > (length x) = " |"
    | otherwise = (printWinBoard1 (tail x) (p-1))

    -- if P2 is winner
printWinBoard2 :: [Player] -> Int -> String
printWinBoard2 [] p = " |"
printWinBoard2 x 1 
    | head x == P1 = "X|"
    | head x == P2 = "*|"
    | otherwise = " |"
printWinBoard2 x p 
    | p > (length x) = " |"
    | otherwise = (printWinBoard2 (tail x) (p-1))

--seperated the cells of one row
-- if not a winning board
printBoardHelper :: [[Player]] -> Int -> Int -> String
printBoardHelper [x] p win
    | win == 1 = printWinBoard1 x p
    | win == 2 = printWinBoard2 x p
    | otherwise = printNoWinBoard x p
printBoardHelper (x:t) p win
    | win == 1 = printWinBoard1 x p ++ printBoardHelper t p win
    | win == 2 = printWinBoard2 x p ++ printBoardHelper t p win
    | otherwise = printNoWinBoard x p ++ printBoardHelper t p win

-- joins the rows together, original table is created from columns !
-- if not a winning board
printBoard :: Board -> Int -> Int -> String
printBoard (Tile x (r, c)) p win | p >= (c-1) = "|" ++ printBoardHelper x p win ++ "\n"
         | otherwise = printBoard (Tile x (r, c)) (p+1) win ++ "|"  ++ printBoardHelper x p win ++ "\n"

-- checks if the move is legal
--
isLegalMove:: Board -> Column -> Bool
isLegalMove (Tile [] (r, c)) col = True
isLegalMove bd@(Tile b (r, c)) col 
    | col >= c = False
    | otherwise = isLegalMoveHelper bd col

-- checks if the move is within the rows of the board
isLegalMoveHelper :: Board -> Column -> Bool
isLegalMoveHelper (Tile bd (r,c)) col = (length (bd !! col) < r)

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
play board | checkWin board == 1 = putStrLn "Player 1 wins!"
           | checkWin board == 2 = putStrLn "Player 2 wins!"
           -- showBoard 2 board
           | isDraw board == True = putStrLn "Draw. Game Over."
           | otherwise = do
             x <- getMove (turn board)
             ok <- checkMove board x
             let board1 | ok  = makeMove board x (turn board)
                        | otherwise = board
             temp <- putStrLn ("\n" ++ (showBoard board1))
             play board1


--convert False to a String: Illegal Move!
--
convert :: Bool -> Board -> String
convert bool bd
    | bool == True = ""
    | bool ==  False = 
        "Uh Oh! That move is outside of the board! Try Again " ++ 
        (showPlayer (turn(bd))) ++ "! \n"
    

--checkMove, check the move is valid or not, reture True or False
--
checkMove :: Board -> Column -> IO Bool
checkMove b c = do
    let x = isLegalMove b c
    putStr (convert x b) 
    return (x)

--Prompt for user imput for each turn
--
getMove :: Player -> IO Int
getMove player = do
    putStrLn ""
    putStrLn ("Please enter move for " ++ (showPlayer player)  ++ ": ")
    column <-getLine
    return (read column)


-- type main to start the game
main :: IO ()
-- row and col are variables to make it more easier to 
-- change later on if we want to modify the board size
row = 6
col = 7
main = play (newBoard (row,col))
{-
test1 = True
test2 = False
test3 :: Int -> Bool
test3 i = do
    
    return False
-}

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
    do
        if piece == P1 || piece == P2
    do
        if piece == P1 || piece == P2
        else
        iterateColumns (columns - 1)

-}