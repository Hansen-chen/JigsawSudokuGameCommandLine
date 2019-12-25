module Main where

-- deal with stack tool when implementing GUI  
import Data.Array
import System.IO

-- First matrix records number in the Board, -1 as not filled
-- The second matrix records the board location
data Board = Board (Array (Int, Int) Int) (Array (Int, Int) Int) deriving (Eq)

printArray :: ( Array (Int,Int) Int ) -> String
printArray arr = unlines [unwords [show (arr ! (x, y)) | x <- [0..8]] | y <- [0..8]]

printArraySave :: ( Array (Int,Int) Int ) -> [Char]
printArraySave arr = unlines [unwords [if (arr ! (x, y)) >= 0 then show (arr ! (x, y)) else show ('.') | x <- [0..8]] | y <- [0..8]]

instance Show Board where
    show (Board num loc) = "number: \n" ++ printArray num ++ "\n location: \n" ++ printArray loc

-- Load Board
load = readFile "../../final project/map.txt" >>= 
        return  . map scanString . lines >>= \b -> 
            return (Board (array ((0,0),(8,8)) (arrayConstructor (map ((!!) b) [9..17]))) (array ((0,0),(8,8)) (arrayConstructor (map ((!!) b) [0..8]))))

-- Display Board
-- TODO: output as in pdf file / GUI
displayBoard :: Board -> [Char]
displayBoard board = show board

saveBoardFormat :: Board -> [Char]
saveBoardFormat (Board num loc) = filter (\x -> (x /=' ' && x/='\'')) (printArraySave loc ++ printArraySave num)

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

-- Make Move with Jigsaw Sudoku game rules checking 
move :: Board -> Int -> Int -> Int -> Board
move (Board num loc) x y n | check (Board num loc) x y n = Board (num // [((x,y), n)]) loc
                            | otherwise = (Board num loc)

-- Implement checking including the range of location(0-8), number to be inserted (1-9), the location is empty or not,
-- and the same line/row contains the number you are trying to insert or not.                            
check :: Board -> Int -> Int -> Int -> Bool
check (Board num loc) x y n = 0 <= x && x <= 8 && 0 <= y && y <= 8 && 1 <= n && n <= 9 && (num ! (x,y)) == -1
                                   && not (elem n (map (\((_,_),z) -> z) (filter (\((a,_),_) -> a == x) (assocs num))))
                                   && not (elem n (map (\((_,_),z) -> z) (filter (\((_,a),_) -> a == y) (assocs num))))
                                   && jigsawBlockCheck (Board num loc) x y n

-- The same Jigsaw Sudoku block does not contain the number you are trying to insert.
jigsawBlockCheck :: Board -> Int -> Int -> Int -> Bool                                  
jigsawBlockCheck (Board num loc) x y n = not (elem n (map ((!) num) (map (\((a,b),_) -> (a,b)) (filter (\((_,_),c) -> c == loc ! (x,y)) (assocs loc)))))

-- Decide whether the player win or not
jigsawSudokuCheck :: Board -> Bool
jigsawSudokuCheck (Board num loc) = not (elem (-1) (elems num))

-- Save Board (may change to function)
-- Done in command system
-- Checked by: readFile "file.txt" >>= \a -> readFile "map.txt" >>= \b -> return (a==b)

-- main access point
main :: IO ()                    
main = putStrLn "Jigsaw Sudoku">>= \_ ->
            load >>= \b -> play b

-- demo play game
play :: Board -> IO ()
play sudoku = putStr (displayBoard sudoku) >>= \_ ->
                putStrLn "Please enter command: " >>= \_ ->
                    getLine >>= \cmd ->
                        case cmd of 
                            "move" -> putStrLn "Row: " >>= \_ -> getChar >>= \y -> putStrLn "Column: " >>= \_ -> getChar >>= \x -> putStrLn "Number: " >>= \_ -> getChar >>= \n ->
                                                    return (move sudoku (scanChar x) (scanChar y) (scanChar n)) >>= \b ->
                                                        if (jigsawSudokuCheck b) then
                                                            putStrLn "You win!"
                                                         else 
                                                            play b
                            "quit" -> putStrLn "Bye"
                            "save" -> writeFile "file.txt" (saveBoardFormat sudoku) >>= \_ -> putStrLn "Saved"
                            _ -> play sudoku                                

-- TODO: redo
