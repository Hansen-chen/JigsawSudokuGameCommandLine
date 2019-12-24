module Main where

-- deal with stack tool when implementing GUI  
import Data.Array
import System.IO


-- construction
-- table = array ((1,1),(3,3)) [ ((1,1), [1,2]), ((1,2), [1,2]), ((1,3), [1,2]), ((2,1), [1,2]), ((2,2), [1,2]), ((2,3), [1,2]), ((3,1), [1,2]), ((3,2), [1,2]), ((3,3), [1,2]) ]

-- modification
-- table = table // [((1,1), '1')]

-- access
-- table ! (1,1)


-- First matrix records number in the Board, -1 as not filled
-- The second matrix records the board location
data Board = Board (Array (Int, Int) Int) (Array (Int, Int) Int) deriving (Eq)

-- Display Board
-- TODO: output as in pdf file
printArray :: ( Array (Int,Int) Int ) -> String
printArray arr = unlines [unwords [show (arr ! (x, y)) | x <- [0..8]] | y <- [0..8]]

instance Show Board where
    show (Board num loc) = "number: \n" ++ printArray num ++ "\n location: \n" ++ printArray loc
  
-- other checking should be defined
-- https://www.tutorialspoint.com/haskell/haskell_types_and_type_class.htm
-- last one

-- Load Board
rawBoard = readFile "../../final project/map.txt" >>= return  . map scanString . lines

load = rawBoard >>= \b -> return (Board (array ((0,0),(8,8)) (arrayConstructor (map ((!!) b) [9..17]))) (array ((0,0),(8,8)) (arrayConstructor (map ((!!) b) [0..8]))))

testMove = load >>= \b -> return (move b 1 1 2)

scanChar :: Char -> Int
scanChar c | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
           | otherwise = -1

scanString :: String -> [Int]
scanString [] = []
scanString (x:xs) | 0 <= sc && sc <= 9 = sc:(scanString xs)
                  | otherwise = -1:(scanString xs)
                    where sc = scanChar x

arrayConstructor :: [[Int]] -> [((Int, Int), Int)]
arrayConstructor [[]] = [((0,0),0)]
arrayConstructor m = [ ((x,y),m!!y!!x) | x <- [0..8], y <- [0..8] ]                   

-- Make Move
move :: Board -> Int -> Int -> Int -> Board
move (Board num loc) x y n | 0 <= x && x <= 8 
                                && 0 <= y && y <= 8 
                                && 1 <= n && n <= 9 
                                = Board (num // [((x,y), n)]) loc
                            | otherwise = (Board num loc)

-- TODO: further error handling for make move

-- Save Board
-- TODO: write file with Board Object                    

-- main access point                    
main = putStrLn "Jigsaw Sudoku">>=
        return load

