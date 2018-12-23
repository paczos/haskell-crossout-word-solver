import System.Environment
import System.IO
import Data.List
import Data.String
import Data.Char

main = do
    (puzzleBoard:puzzleWords:_) <- getArgs
    putStrLn "The arguments are:"
    putStrLn puzzleBoard
    putStrLn puzzleWords
    putStrLn "\n"
    boardRC <- readBoard puzzleBoard
    print boardRC
    putStrLn "\n"
    words <- readWords puzzleWords 
    print words
    prettyPrintBoard boardRC

-- the output of this function is a 3-tuple of (board, numRows, numCols)   
readBoard fileName = do
    contents <- readFile fileName
    -- putStr contents
    let numRows = length . lines
    let numCols = length . filter (\x -> isAsciiUpper x) . head . lines
    -- print (numRow contents)
    -- print (numCols contents)
    let board = filter (\x -> isAsciiUpper x) contents
    -- print board
    return (board, (numRows contents), (numCols contents))

-- the output of this function is an array of words
readWords fileName = do
    contents <- readFile fileName
    -- putStrLn contents
    return (lines (filter (\x -> isAsciiUpper x || x == '\n') contents))

-- just some better printing, takes in a tuple (board, row, column)
prettyPrintBoard boardRC = do
    putStrLn "\n"
    putStrLn "BOARD STATE:"
    putStrLn $ concat (replicate 20 "-")
    prettyPrintBoard' boardRC 
    where
        prettyPrintBoard' ([], _, _) = putStr []
        prettyPrintBoard' (board, row, column) = do
            putStrLn (intersperse ' ' (take column board))
            prettyPrintBoard' ((drop column board), row, column)
