-- Omok - Haskell
-- Author: Efrain Retana
--

module Main (main) where

import Board

-- Main method
main :: IO ()
main = do
    let bd = mkBoard 15
    putStrLn (boardToStr playerToChar bd)
    playGame bd
    where
        playGame bd = do
            playerMove <- readXY bd 1
            let playerBoardMove = uncurry mark playerMove bd 1
            putStrLn (boardToStr playerToChar playerBoardMove)
            if isWonBy playerBoardMove 1 then do
                putStrLn "Player 1 wins!"
                return ()
            else do
                opponentCoordinates <- readXY playerBoardMove 2
                let bdOpponentMove = uncurry mark opponentCoordinates playerBoardMove 2
                putStrLn (boardToStr playerToChar bdOpponentMove)
                if isWonBy bdOpponentMove 2 then do
                    putStrLn "Player 2 is the winner!"
                    return ()
                else if isDraw bdOpponentMove then do
                    putStrLn "Game is a draw!"
                    return ()
                else do
                    playGame bdOpponentMove

-- Turns a given player into a character
playerToChar :: Int -> Char
playerToChar x
    | x == 1 = 'O'
    | x == 2 = 'X'
    | otherwise = '.'

-- Read the coordinates of a player
readXY:: [[Int]] -> Int -> IO(Int, Int)
readXY bd p = do
    putStrLn ("Player " ++ show p ++ ": Enter x and y coordinates separated by spaces. (Use values between 1 and " ++ show(size bd) ++ ")")
    line <- getLine
    let enteredValues = map read (words line)
    if length enteredValues /= 2  then do
        putStrLn "Malformed input, Please input two values separated by a space."
        readXY bd p
    else if head enteredValues < 1 || head enteredValues > size bd || enteredValues!!1 < 0 || enteredValues!!1 > size bd then do
        putStrLn "Coordinates invalid. Try again."
        readXY bd p
    else if not (isEmpty (head enteredValues) (enteredValues!!1) bd) then do
        putStrLn "Invalid spot. Try again."
        readXY bd p
    else
        return (head enteredValues, enteredValues!!1)

