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

-- make new board from row and column
-- (replicate col []) creates a list of length c 
newBoard:: (Row, Column) -> Board
newBoard (r, c) = Tile (replicate c []) (r, c)

showPlayer :: Player -> [Char]
showPlayer P1 = "P1"
showPlayer P2 = "P2"
--showPLayer W = "W"

-- turns board to string
showBoard :: Board -> String
showBoard b@(Tile x (r,c))
    | checkWin b == 0 = (printBoard b 1 0) ++ printBottom (c-1)
    | checkWin b == 1 = (printBoard b 1 1) ++ printBottom (c-1)
    | checkWin b == 2 = (printBoard b 1 2) ++ printBottom (c-1)

-- nextPlayer P1 will return P2 and vice versa
nextPlayer :: Player -> Player
nextPlayer player 
    | player == P1 = P2
    | player == P2 = P1


-- changePlayer :: Player -> Player
-- changePlayer p = W

-- CHECK FOR WINS:

-- checks if there is a winner
checkWin :: Board -> Int
checkWin bd 
    | checkWinIter bd P1 = 1
    | checkWinIter bd P2 = 2
    | otherwise = 0

--checks diagonals, horizontal, and vertical wins. use transpose of board to flip it
checkWinIter :: Board -> Player -> Bool
checkWinIter x p = (checkLeftDiag x p) || (checkRightDiag x p) || (checkVertical x p) || (checkVertical (transboard x) p)

-- checks if board full (if yes, then it's a tie)
checkTie :: Board -> Bool
checkTie (Tile [] z) = True
checkTie (Tile (x:xs) (r, c))
    | length x == r && checkTie (Tile xs (r,c)) == True = True
    | otherwise = False

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
    | y < length colLength = colLength !! y
    | otherwise = player
    where colLength | x < length bd = bd !! x
                    | otherwise = []

-- checks for right diagonal win
checkRightCoin :: Board -> Player -> Int -> Int -> Bool
checkRightCoin bd player x y = t1 == player && t2 == player && t3 == player && t4 == player 
    where 
        player2 = nextPlayer player
        t1 = getTile bd x y player
        t2 = getTile bd (x+1) (y+1) player2
        t3 = getTile bd (x+2) (y+2) player2
        t4 = getTile bd (x+3) (y+3) player2

checkRightDiagHelper :: Board -> Player -> Int -> Int -> Bool
checkRightDiagHelper bd@(Tile b (r, c)) p x y 
    | x > c-4 && y > r - 5 = False
    | x > c-4 = checkRightDiagHelper bd p 0 (y+1) 
    | otherwise = checkRightCoin bd p x y || checkRightDiagHelper bd p (x+1) y

-- checks for a right diagonal win (/)
checkRightDiag :: Board -> Player -> Bool
checkRightDiag  bd p = checkRightDiagHelper  bd p 0 0

--checks for a left diagonal win (\)
checkLeftCoin :: Board -> Player -> Int -> Int -> Bool
checkLeftCoin bd player x y = do
    t1 == player && t2 == player && t3 == player && t4 == player
    where
        player2 = nextPlayer player
        t1 = getTile bd x y player2
        t2 = getTile bd (x-1) (y+1) player2
        t3 = getTile bd (x-2) (y+2) player2
        t4 = getTile bd (x-3) (y+3) player2

-- checkLeftDiagHelper takes in a board, player, x position of coin, y position of coin and checks for \
-- if the bottom coin is on the bottom row, then the minimum x position is 3
-- which is why checkLeftDiag passes a = 3, b = 0
checkLeftDiagHelper :: Board -> Player -> Int -> Int -> Bool
checkLeftDiagHelper bd@(Tile b (r,c)) player x y 
            | x >= c && y < (r-4) = checkLeftDiagHelper bd player middle (y+1)
            | x >= c && y >= (r-4) = False
            | otherwise = checkLeftCoin bd player x y || checkLeftDiagHelper bd player (x+1) y
            where middle = 3

--checks (goes into checkLeftDiagHelper) if columns is greater than 3, as you need at least 4 columns for a diagonal connect
-- and with our standard board, we have 7 columns, we will check for diagonals starting from the case where coin is in midle bottom placement
checkLeftDiag :: Board -> Player -> Bool
checkLeftDiag  (Tile x (r, c))  p 
    | c > 3 = checkLeftDiagHelper (Tile x (r,c)) p middle 0       
    | otherwise = False
    where middle = 3

-- tranposes the board, switches columns into rows, and vice versa
transboard :: Board -> Board
transboard y@(Tile x (r,c)) = player2board ( transpose ( board2player y)) r c

-- makes array of players from board
board2player :: Board -> [[Player]]
board2player (Tile x (row, col)) = x

-- makes board from array of players, row, and col
player2board :: [[Player]] -> Row -> Column -> Board
player2board x r c = (Tile x (r,c))


-- generates the bottom lines of the board
printBottom :: Int -> String
printBottom a 
    | a == -1 = "+\n"
    | otherwise = "+-" ++ printBottom (a-1) ++ " " ++ (show a)


-- TODO: to change the symbols for the winning player, we need to change this function.
--creates output for each cell

-- Print non-winning board
printNoWinBoard :: [Player] -> Int -> String
printNoWinBoard [] p = " |"
printNoWinBoard x 1 
    | head x == P1 = "X|"
    | head x == P2 = "O|"
--  | head x == W = "* |"
    | otherwise = " |"
printNoWinBoard x p 
    | p > (length x) = " |"
    | otherwise = (printNoWinBoard (tail x) (p-1))

-- Print board if P1 is winner
-- changes all P1 to *
printWinBoard1 :: [Player] -> Int -> String
printWinBoard1 [] p = " |"
printWinBoard1 x 1 
    | head x == P1 = "*|"
    | head x == P2 = "O|"
    | otherwise = " |"
printWinBoard1 x p 
    | p > (length x) = " |"
    | otherwise = (printWinBoard1 (tail x) (p-1))

-- Print board if P2 is winner
-- changes all P2 to *
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

-- joins the rows together, original table is created from columns!
-- if not a winning board
printBoard :: Board -> Int -> Int -> String
printBoard (Tile x (r, c)) tile win 
    | tile >= (c-1) = "|" ++ printBoardHelper x tile win ++ "\n"
    | otherwise =  printBoard (Tile x (r, c)) (tile+1) win ++ "|"  ++ printBoardHelper x tile win ++ "\n"

-- make one move, raise errors for illegal move
doMove :: Board -> Column -> Player -> Board
doMove y@(Tile board (r, c)) column player = Tile (first ++ move ++ second) (r, c)
                -- first is the beginning of the board up to the column player chose
          where first  = (firstBoard board column)
                -- move is the column that the player chose
                move  = [((board !! column) ++ [player])]
                -- second is the rest of the board after the column the player chose
                second = (secondBoard board column)

-- returns the board up to the column chosen by player
firstBoard:: [[Player]] -> Int -> [[Player]]
firstBoard board column = take column board

--returns the board from after the column chosen to the end
secondBoard:: [[Player]] -> Int -> [[Player]]
secondBoard board c = drop (c + 1) board

-- if Boolean is false, then print error msg to try again, else prints blank.
displayErrorMsg :: Bool -> Board -> String
displayErrorMsg bool bd
    | bool == True = ""
    | bool ==  False = 
        "Uh Oh! That move is outside of the board! Try Again " ++ 
        (showPlayer (turn(bd))) ++ "!"
    
--Find out whose turn it is
turn :: Board -> Player
turn (Tile bd (r, c))  
    | numOfP1 > numOfP2 = P2
    | otherwise = P1
        where
            -- count the number of P1s
            numOfP1 = length (filter (==P1) (concat bd))
            -- count the number of P2s
            numOfP2 = length (filter (==P2) (concat bd))

-- determines if the player's move is within the scope of the board
isMoveInBoard:: Board -> Column -> Bool
isMoveInBoard (Tile [] (r, c)) col = True
isMoveInBoard bd@(Tile b (r, c)) col 
    | col >= c = False
    | otherwise = isMoveInBoardHelper bd col

-- checks if the move is within the rows of the board
isMoveInBoardHelper :: Board -> Column -> Bool
isMoveInBoardHelper (Tile bd (r,c)) col = (length (bd !! col) < r)

-- checks if the player's move is valid
checkMove :: Board -> Column -> IO Bool
checkMove b c = do
    let x = isMoveInBoard b c
    putStrLn (displayErrorMsg x b) 
    return (x)

-- Prompts player to go, and returns the column inserted
getMove :: Player -> IO Int
getMove player = do
    putStrLn ""
    putStrLn ("Please enter move for " ++ (showPlayer player)  ++ ": ")
    column <-getLine
    return (read column)

--- PLAY GAME -----
play :: Board -> IO ()
play board | checkWin board == 1 = putStrLn "Player 1 wins!"
           | checkWin board == 2 = putStrLn "Player 2 wins!"
           | checkTie board == True = putStrLn "Draw. Game Over."
           | otherwise = do
             chosenColumn <- getMove (turn board)
             isValid <- checkMove board chosenColumn
             let nextBoard | isValid  = doMove board chosenColumn (turn board)
                        | otherwise = board
             temp <- putStrLn ("\n" ++ (showBoard nextBoard))
             play nextBoard

-- type main to start the game
main :: IO ()
-- row and col are variables to make it more easier to 
-- change later on if we want to modify the board size
row = 6
col = 7
main = play (newBoard (row,col))

{-
NOTES:
This link was very useful to start our game of connect 4. http://www.cs.nott.ac.uk/~pszgmh/afp-cwk1.pdf
Diagonal checks were taken from https://github.com/maysam/connect-four-haskell/blob/master/Board.hs
-}