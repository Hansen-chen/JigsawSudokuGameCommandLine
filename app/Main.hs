module Main where

-- deal with stack tool    
-- import Data.Array

-- construction
-- table = array ((1,1),(3,3)) [ ((1,1), [1,2]), ((1,2), [1,2]), ((1,3), [1,2]), ((2,1), [1,2]), ((2,2), [1,2]), ((2,3), [1,2]), ((3,1), [1,2]), ((3,2), [1,2]), ((3,3), [1,2]) ]

-- modification
-- table = table // [((1,1), '1')]

-- access
-- table ! (1,1)


-- List of Int records number in the block, -1 as not filled
-- The second list records the board
data Block = Block [[Int]] [[Int]] deriving (Eq)

-- Display Board
-- TODO: output as in pdf file
instance Show Block where
    show (Block loc num) = "location: " ++ show loc ++ "\n number:" ++ show num
  
-- other checking should be defined
-- https://www.tutorialspoint.com/haskell/haskell_types_and_type_class.htm
-- last one

-- Load Board
-- 2d array element access: (a!!1)!!2
rawBoard = readFile "../../final project/map.txt" >>= return  . map scanString . lines

load = rawBoard >>= \b -> return (Block (map ((!!) b) [0..8]) (map ((!!) b) [9..17]))

scanChar :: Char -> Int
scanChar c | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
           | otherwise = -1

scanString :: String -> [Int]
scanString [] = []
scanString (x:xs) | 0 <= sc && sc <= 9 = sc:(scanString xs)
                  | otherwise = -1:(scanString xs)
                    where sc = scanChar x

-- Save Board
-- TODO: write file with Block Object                    

-- Make Move
-- TODO: further error handling?
-- or use matrix
move :: Block -> Int -> Int -> Int -> Block
move (Block loc num) x y n | 0 <= x && x <= 9 
                                && 0 <= y && y <= 9 
                                && 0 <= n && n <= 9 
--                                = Block loc (num & element y . element x .~ n)
                                = (Block loc num)
                            | otherwise = (Block loc num)
                            


-- main access point                    
main = putStrLn "Jigsaw Sudoku">>=
        return load