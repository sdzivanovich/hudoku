module Main where

import Sudoku
import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile . head $ args
    let board = parseBoard . lines $ contents
    let s = case solve board of
                Nothing -> "Unsolvable"
                Just b  -> prettyBoard b
    putStr s
