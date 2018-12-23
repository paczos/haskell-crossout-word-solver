import System.Environment
import System.IO
import Data.List

main = do
    (puzzleBoard:puzzleWords:_) <- getArgs
    putStrLn "The arguments are:"
    putStrLn puzzleBoard
    putStrLn puzzleWords
