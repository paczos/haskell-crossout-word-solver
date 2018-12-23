import System.Environment
import System.IO
import Data.List
import Data.String
import Data.Char

makeOptions w h = (indexesForRows w h) ++ (indexesForColumns w h) ++ (indexesForDiagonalsLeft w h) ++ (indexesForDiagonalsRight w h)
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
    let (board, h, w) = boardRC
    putStrLn (crossOutWords board words (makeOptions w h))
    prettyPrintBoard ((crossOutWords board words (makeOptions w h)), h, w)

indexesForRows w h = [[a + b * w | a <- [0..w-1]] | b <- [0..h-1]]
indexesForColumns w h = [[b + a * w | a <- [0..h-1]] | b <- [0..w-1]]
indexesForDiagonalsRight w h = []    -- TODO: generate each Right-diagonal as an array of indices
indexesForDiagonalsLeft w h  = []    -- TODO: generate each Left-diagonal as an array of indices


isPrefix [] _ = True
isPrefix (x:xs) (t:ts) = if x==t then isPrefix xs ts else False
isPrefix (x:_) [] = False

findPos' (x:xs) (t:ts) idx = if isPrefix (x:xs) (t:ts) then idx 
else findPos' (x:xs) ts (idx + 1)

findPos' [] _  idx = idx
findPos' (_:_) [] _ = -1
findSubstringPos xs ts = findPos' xs ts 0

crossOutSingle' (b:board) crossIdx currIdx acc = if crossIdx == currIdx then (acc ++ ['X'] ++  board)
 else crossOutSingle' board crossIdx (currIdx+1) (acc++[b])

crossOutAt board idx = crossOutSingle' board idx 0 []
option2str board (o:option) = [board !! o] ++ (option2str board option)
option2str _ [] = [] 

crossOutOption board (o:os) = crossOutOption (crossOutAt board o) os
crossOutOption board [] = board
crossOut board (opt:options) = crossOut (crossOutOption board opt) options
crossOut board [] = board

crossOutWord board w (o:options) = if (findSubstringPos w (option2str board o)) /= -1 then crossOutOption board (take (length w) (drop (findSubstringPos w (option2str board o)) o))
    else crossOutWord board w options 
crossOutWord board _ []  = board


crossOutWords board (w:ws) options = crossOutWords (crossOutWord board w options) ws options
crossOutWords board [] _ = board

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
