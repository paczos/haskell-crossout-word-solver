import System.Environment
import System.IO
import Data.List
import Data.String
import Data.Char

makeOptions w h = (indexesForRows w h) ++ (indexesForColumns w h) ++ (indexesForDiagonalsLeft w h) ++ (indexesForDiagonalsRight w h)

type BoardCell = (Char,Bool)
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
    let boardData = fmap (\c -> (c, True)) board
    let resBoard = (crossOutWords boardData words (makeOptions w h))
    let resString = fmap (\(b, s)-> if s == False then '#' else b) resBoard
    prettyPrintBoard (resString, h, w)
    let finalOut = fmap (\(b,s)->b) (filter (\(b,s)-> s==True) resBoard)
    putStrLn finalOut

indexesForRows w h = [[a + b * w | a <- [0..w-1]] | b <- [0..h-1]]
indexesForColumns w h = [[b + a * w | a <- [0..h-1]] | b <- [0..w-1]]

downRightIndex w h index = if i < h - 1 && j < w - 1 then (i + 1) * w + j + 1 else -1 
                where
                    i = quot index w
                    j = mod index w

upRightIndex w h index = if i > 0 && j < w - 1 then (i - 1) * w + j + 1 else -1 
                where
                    i = quot index w
                    j = mod index w

iterateAll (-1) _ = []
iterateAll i f = i : iterateAll next f where
                    next = f (i)

indexesForDiagonalsRight w h = [iterateAll i (downRightIndex w h) | i <- [0..w-1] ++ [i * w | i <- [1..h-1]]]
indexesForDiagonalsLeft w h = [iterateAll i (upRightIndex w h) | i <- [ i * w | i <- [0..h-1]] ++ [(w * (h - 1))..(w * h) - 1]]

isPrefix [] _ = True
isPrefix (x:xs) (t:ts) = if x==t  then isPrefix xs ts else False
isPrefix (x:_) [] = False

findPos' (x:xs) (t:ts) idx = if isPrefix (x:xs) (t:ts) then idx 
else findPos' (x:xs) ts (idx + 1)

findPos' [] _  idx = idx
findPos' (_:_) [] _ = -1
findSubstringPos xs ts = findPos' xs ts 0

crossOutSingle' ((b, s):board) crossIdx currIdx acc = if crossIdx == currIdx then (acc ++ [(b, False)] ++  board)
 else crossOutSingle' board crossIdx (currIdx+1) (acc++[(b,s)])

crossOutAt board idx = crossOutSingle' board idx 0 []
option2str board (o:option) = [fst (board !! o)] ++ (option2str board option)
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
