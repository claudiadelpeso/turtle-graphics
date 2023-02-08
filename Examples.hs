{- |
Module      : Examples
Description : This module contains different examples to show how the functions work. Example 8 is used as the example that runs the main function
-}
module Examples (example8) where 

import Turtle 

-- | This example uses forward (backward) and right (left) actions (use them to make a square with slash in middle)
example1 :: [Instruction]
example1 = [forward 100, right 1.5708, forward 100, right 1.5708,forward 100, right 1.5708, forward 100, left 0.785398, backward 140]

-- | This example uses penup and pendow on the square example1. It also changes color each time it draws. This creates a S kind of shape. 
example2 :: [Instruction]
example2 = [color (RGB 0 0 255), forward 100, right 1.5708, penup, forward 100, right 1.5708, pendown, color (RGB 255 0 0), forward 100, right 1.5708, penup,  forward 100, left 0.785398, pendown, color (RGB 0 255 0), backward 140]

-- | This example shows how command die kills turtle
example3 :: [Instruction]
example3 = [forward 100, right 1.5708, penup, forward 100, die, right 1.5708, pendown, forward 100, right 1.5708, pendown, forward 100, left 0.785398, backward 140]

-- | Program that does not terminate 
example4 :: [Instruction]
example4 = [forward 100, color (RGB 255 255 0), idle, forever (forward 100), die, right 1.5708, pendown, forward 100, right 1.5708, pendown, forward 100, left 0.785398, backward 140]

-- | Using Parallel composition
example5 :: [Instruction]
example5 = [forward 10, right 45, pendown, (<|>) (forward 200) ((<|>) (right 20) (color (RGB 255 255 0)))]
example6 :: [Instruction]
example6 = [forward 10, right 45, pendown, (<|>) (forward 200) ((<|>) (forward 200) die), right 90, color (RGB 0 255 0), backward 100]

-- | Program that repeats an action and uses limited
example7 :: [Instruction]
example7 = [forward 10, right 45, pendown, (<|>) (limited 3 [(forward 100),backward 200, color (RGB 0 0 255)]) (limited 3 [(forward 100),backward 200, color (RGB 10 100 200)]), right 10, idle, color (RGB 255 255 0), backward 100] 


-- | A program that uses everything 
example8 :: [Instruction]
example8 = [forward 100, right 1.5708, times 2 (forward 50), right 1.5708, forward 100, right 1.5708, times 2 (forward 50), right 1.5708, color (RGB 0 0 255),  (>*>) (right 2.35619) (backward 70.7107), right 1.5708, backward 70.7107]
