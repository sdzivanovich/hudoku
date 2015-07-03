module Sudoku
( Board
, solve
, parseBoard
, prettyBoard
) where

import Data.Char(digitToInt, isDigit)
import Data.List(intercalate)
import Data.Maybe(isJust)
import qualified Data.Set as Set
import qualified Data.Vector as Vec

data Cell = Filled Int | Empty
            deriving (Eq, Show)

type Board = Vec.Vector (Vec.Vector Cell)

solve :: Board -> Maybe Board
solve b
    | isSolved b = Just b
    | otherwise  = indexOfNextEmpty b >>= solvePosition b

solvePosition :: Board -> (Int, Int) -> Maybe Board
solvePosition b c = let attempts = map Filled . Set.toList . possibilities b $ c
                        results = filter isJust . map (solve . updateBoard b c) $ attempts
                    in  if null results then Nothing else head results

indexOfNextEmpty :: Board -> Maybe (Int, Int)
indexOfNextEmpty b = indexOfNextEmptyFrom b 0

indexOfNextEmptyFrom :: Board -> Int -> Maybe (Int, Int)
indexOfNextEmptyFrom b i
    | i == Vec.length b = Nothing
    | otherwise         = case Vec.elemIndex Empty (b Vec.! i) of
                            Nothing -> indexOfNextEmptyFrom b (i + 1)
                            Just x  -> Just (i, x)

isSolved :: Board -> Bool
isSolved b =    let groupings = getAllRows b ++ getAllColumns b ++ getAllGroups b
                in  all (== target) . map cellsToNumbers $ groupings

prettyBoard :: Board -> String
prettyBoard = ( ++ "\n") . intercalate "\n" . map (concatMap prettyCell . Vec.toList) . Vec.toList

prettyCell :: Cell -> String
prettyCell (Filled x)   = show x
prettyCell _            = "-"

updateBoard :: Board -> (Int, Int) -> Cell -> Board
updateBoard b (row, col) c =    let newRow = getRow b row Vec.// [(col, c)]
                                in  b Vec.// [(row, newRow)]

parseBoard :: [String] -> Board
parseBoard ss = Vec.fromList (map parseRow ss)

parseRow :: String -> Vec.Vector Cell
parseRow s = Vec.fromList (map parseCell s)

parseCell :: Char -> Cell
parseCell c
    | isDigit c = Filled (digitToInt c)
    | otherwise = Empty

getAllRows :: Board -> [Vec.Vector Cell]
getAllRows b = map (getRow b) [0..blen]

getAllColumns :: Board -> [Vec.Vector Cell]
getAllColumns b = map (getColumn b) [0..blen]

getAllGroups :: Board -> [Vec.Vector Cell]
getAllGroups b =    let indices = [0, 3, 6]
                    in  map (getGroup b) [(x, y) | x <- indices, y <- indices]

getRow :: Board -> Int -> Vec.Vector Cell
getRow = (Vec.!)

getColumn :: Board -> Int -> Vec.Vector Cell
getColumn b col = Vec.map (Vec.! col) b

getGroup :: Board -> (Int, Int) -> Vec.Vector Cell
getGroup b c =  let (beginRow, beginCol) = beginGroupIndices c
                    rows = Vec.slice beginRow 3 b
                in  Vec.concatMap (Vec.slice beginCol 3) rows

beginGroupIndices :: (Int, Int) -> (Int, Int)
beginGroupIndices (row, col) = (beginGroupIndex row, beginGroupIndex col)

beginGroupIndex :: Int -> Int
beginGroupIndex i = head . filter (\x -> x `mod` 3 == 0) $ [i, i - 1..0]

possibilities :: Board -> (Int, Int) -> Set.Set Int
possibilities b c@(row, col) =  let rowNumbers   = numbersInRow b row
                                    colNumbers   = numbersInCol b col
                                    groupNumbers = numbersInGroup b c
                                in  target Set.\\ Set.unions [rowNumbers, colNumbers, groupNumbers]

numbersInRow :: Board -> Int -> Set.Set Int
numbersInRow = extractNumbers getRow

numbersInCol :: Board -> Int -> Set.Set Int
numbersInCol = extractNumbers getColumn

numbersInGroup :: Board -> (Int, Int) -> Set.Set Int
numbersInGroup = extractNumbers getGroup

extractNumbers :: (Board -> a -> Vec.Vector Cell) -> Board -> a -> Set.Set Int
extractNumbers f b = cellsToNumbers . f b

cellsToNumbers :: Vec.Vector Cell -> Set.Set Int
cellsToNumbers = Set.fromList . Vec.toList . Vec.map cellToInt . Vec.filter isFilled

cellToInt :: Cell -> Int
cellToInt (Filled x)  = x
cellToInt _           = error "cannot convert empty to int"

isFilled :: Cell -> Bool
isFilled (Filled _) = True
isFilled _          = False

blen :: Int
blen = 8

target :: Set.Set Int
target = Set.fromList [1..9]
