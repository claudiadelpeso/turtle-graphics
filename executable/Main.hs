{- |
Module      : Main
Description : In this main module we define the initial turtle and program which we want to run 
-}
module Main where

import Turtle (Turtles(..), Color(..),runTextual, runGraphical)
import Example (example8)
import qualified Graphics.HGL as HGL

main :: IO()
main = 
    let initTurtle = Turtles {pos = (250,250), angle = 0, pen = True, col = (RGB 255 255 255), time = 0, dead = False}
        program = example8
    -- in runTextual initTurtle program
    in runGraphical initTurtle program

