-- Omok - Haskell
-- Author: Efrain Retana
--

module Board where

-- Generate board given n
mkBoard :: Int -> [[Int]]
mkBoard 0 = [[]]
mkBoard n = mkBoardHelper n n

-- Generate 2d list
mkBoardHelper :: Int -> Int -> [[Int]]
mkBoardHelper 1 n  = [mkRow n]
mkBoardHelper m n = mkRow n : mkBoardHelper (m-1) n

-- Generate rows of board
mkRow :: Int -> [Int]
mkRow 0 = []
mkRow n = 0 : mkRow (n-1)

-- Return a player
mkPlayer :: Int
mkPlayer = 1

-- Return opponent
mkOpponent :: Int
mkOpponent = 2

-- Size of board
size :: [[Int]] -> Int
size = length

-- Return row of board
row :: Int -> [[Int]] -> [Int]
row y bd = bd !! (y - 1)

-- Return column of board
column :: Int -> [[Int]] -> [Int]
column x bd = [row n bd !! (x-1) | n <- [1..size bd]]


-- Part 2 of Haskell Omok
mark:: Int -> Int ->  [[Int]] -> Int -> [[Int]]
mark x y (h:t) p
    | y == 1 = markRow x h p : t
    | otherwise = h: mark x (y-1) t p

-- Mark helper to mark a row
markRow:: Int -> [Int] -> Int -> [Int]
markRow _ [] _ = []
markRow n (h:t) p
    | n == 1 = p : markRow (n-1) t p
    | otherwise = h : markRow (n-1) t p

-- Check if spot is empty
isEmpty :: Int -> Int -> [[Int]] -> Bool
isEmpty x y (h:t)
    | y == 1 = isEmptyRow x h
    | otherwise = isEmpty x (y-1) t

-- Helper for isEmpty function
isEmptyRow :: Int -> [Int] -> Bool
isEmptyRow _ [] = False
isEmptyRow x (h:t)
    | x == 1 && h == 0 = True
    | otherwise = isEmptyRow (x-1) t

-- Check if current spot is marked or not
isMarked :: Int -> Int -> [[Int]] -> Bool
isMarked x y bd = not (isEmpty x y bd)

-- Check if spot is marked by a player p 
isMarkedBy :: Int -> Int -> [[Int]] -> Int -> Bool
isMarkedBy x y bd p
    | isEmpty x y bd = False
    | otherwise = row y bd !! (x - 1) == p

-- Return the player who's puck exists in this position
marker :: Int -> Int -> [[Int]] -> Int
marker x y bd
    | isMarkedBy x y bd mkPlayer = 1
    | isMarkedBy x y bd mkOpponent = 2
    | otherwise = 0

isFull :: [[Int]] -> Bool
isFull [[]] = True
isFull bd = 0 `notElem` concat bd


-- Part 3
isWonBy :: [[Int]] -> Int -> Bool
isWonBy bd p
    | or [hasWinSeq (row y bd) p | y<-[1..size bd]] = True -- Check left and right
    | or [hasWinSeq (column x bd) p | x <- [1..size bd]] = True -- Check up and down
    | or [hasWinSeq (row y (diagonals bd)) p | y<-[1..size (diagonals bd)]] = True -- check left diagonal
    | or [hasWinSeq (row y (diagonals(reverse bd))) p | y<-[1..size(diagonals bd)]] = True -- Check right diagonal
    | otherwise = False

-- Helper method to check if player has winning sequence in row
hasWinSeq :: [Int] -> Int -> Bool
hasWinSeq [] _ = False
hasWinSeq (h:t) p
    | isEq (take 4 t) p && h == p = True
    | otherwise = hasWinSeq t p
     where
        isEq :: [Int] -> Int -> Bool
        isEq [] _ = True
        isEq (h:t) p
            | h == p = isEq t p
            | otherwise = False

-- Helper method to get the diagonals
diagonals :: [[Int]] -> [[Int]]
diagonals [] = []
diagonals ([]:t) = t
diagonals bd = zipWith (++) (map ((:[]) . head) bd ++ repeat []) ([]:diagonals (map tail bd))

-- Check if game was draw
isDraw :: [[Int]] -> Bool
isDraw = isFull

-- Check if game has terminated
isGameOver :: [[Int]] -> Bool
isGameOver bd 
    | isDraw bd = True
    | isWonBy bd mkPlayer = True
    | isWonBy bd mkOpponent = True
    | otherwise = False


-- Part 4 

-- Gets a string of the board
boardToStr:: (Int -> Char) -> [[Int]] -> String
boardToStr f [] = "\n"
boardToStr f (h : t) = boardToStrHelper f h ++ "\n" ++ boardToStr f t

-- Gets a string of a row in a board
boardToStrHelper:: (Int -> Char) -> [Int] -> String
boardToStrHelper _ [] = ""
boardToStrHelper f (h: t) = [f h] ++ " " ++ boardToStrHelper f t
    






