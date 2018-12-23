import System.Environment
import System.IO
import Data.List

main = do
    --putStrLn (concat (indexesForRows 3 4))
    let w =  (12) -- TODO: calculate it using input data
    let h =  (3)-- TODO: calculate it using input data
    let exWords = ["nice", "rice", "dice"]
    let options = (indexesForRows w h) ++ (indexesForColumns w h) ++ (indexesForDiagonalsLeft w h)++ (indexesForDiagonalsRight w h)
    let exBoard = ['1','2', '3', 'n', 'a', 'n', 'i', 'c', 'e', 'e', 'l', 'n', 
                   '1','d', 'i', 'c', 'e', 's', 'i', 'c', 'e', 'a', 's', '3',
                   '1','w', 'i', 'c', 'e', 's', 'i', 'c', 'r', 'i', 'c', 'e']
    putStrLn (crossOutWords exBoard exWords options)

indexesForRows w h = [[a+b*w | a <- [0..w-1]] | b<-[0..h-1]  ]
indexesForColumns w h = [[a+b*h | a <- [0..h-1]] | b<-[0..w-1]  ]
indexesForDiagonalsRight w h = []    -- generate each Right-diagonal as an array of indices
indexesForDiagonalsLeft w h  = []    -- generate each Left-diagonal as an array of indices


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
option2str _  [] = [] 

crossOutOption board (o:os) = crossOutOption (crossOutAt board o) os
crossOutOption board [] = board
crossOut board (opt:options) = crossOut (crossOutOption board opt) options
crossOut board [] = board 

crossOutWord board w (o:options)  = if (findSubstringPos w (option2str board o)) /= -1 then (crossOutOption board (take (length w) (drop (findSubstringPos w (option2str board o)) o)))
    else crossOutWord board w options 
crossOutWord board _ []  = board 


crossOutWords  board (w:ws) options = crossOutWords (crossOutWord board w options) ws options
crossOutWords board [] _ = board