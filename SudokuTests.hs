module Main where

import Sudoku
import Test.HUnit

test_solveOnSolvedBoardDoesNotChangeBoard :: Test
test_solveOnSolvedBoardDoesNotChangeBoard =
    let boardStr    =   [ "845126379"
                        , "693745812"
                        , "712389465"
                        , "234967581"
                        , "167258934"
                        , "958413627"
                        , "479632158"
                        , "526891743"
                        , "381574296"]
        board       =   parseBoard boardStr
    in TestCase $ assertEqual
        "Solving a solved board should not change the board"
        (Just board)
        (solve board)

test_solveOnUnsolvedBoard :: Test
test_solveOnUnsolvedBoard =
    let boardStr    =   [ "*1*42***5"
                        , "**2*71*39"
                        , "*******4*"
                        , "2*71****6"
                        , "****4****"
                        , "6****74*3"
                        , "*7*******"
                        , "12*73*5**"
                        , "3***82*7*"]

        solutionStr =   [ "813429765"
                        , "462571839"
                        , "795368142"
                        , "247153986"
                        , "539846217"
                        , "681297453"
                        , "978615324"
                        , "126734598"
                        , "354982671"]

        board       =   parseBoard boardStr
        solvedBoard =   parseBoard solutionStr
    in TestCase $ assertEqual
        "Board should solve correctly"
        (Just solvedBoard)
        (solve board)

main :: IO Counts
main = runTestTT $ TestList [ test_solveOnSolvedBoardDoesNotChangeBoard
                            , test_solveOnUnsolvedBoard]
