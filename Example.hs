{- |
Module      : Examples
Description : This module contains different examples to show how the functions work. Example 8 is used as the example that runs the main function
-}
module Example (example8) where 

import Turtle (Program, Color(..), forward, right, left, backward, color, penup, pendown, die, idle, forever, times, limited, lifespan, (>*>), (<|>))

-- | This example uses forward (backward) and right (left) actions (use them to make a square with slash in middle)
example1 :: Program
example1 = forward 100 >*> right 1.5708 >*> forward 100 >*> right 1.5708 >*> forward 100 >*> right 1.5708 >*> forward 100 >*> left 0.785398 >*> backward 140

-- | This example uses penup and pendow on the square example1. It also changes color each time it draws. This creates a S kind of shape. 
example2 :: Program
example2 = color (RGB 0 0 255) >*> forward 100 >*> right 1.5708 >*> penup >*> forward 100 >*> right 1.5708 >*> pendown >*> color (RGB 255 0 0) >*> forward 100 >*> right 1.5708 >*> penup >*> forward 100 >*> left 0.785398 >*> pendown >*> color (RGB 0 255 0) >*> backward 140

-- | This example shows how command die kills turtle. It does not perform any actions after die. 
example3 :: Program
example3 = forward 100 >*> right 1.5708 >*> penup >*> forward 100 >*> die >*> right 1.5708 >*> pendown >*> forward 100 >*> right 1.5708 >*> pendown >*> forward 100 >*> left 0.785398 >*> backward 140

-- | Program that does not terminate (runs for ever)
example4 :: Program
example4 = forward 100 >*> color (RGB 255 255 0) >*> idle >*> forever (forward 100) >*> die

-- | This example shows the use of limited. It runs the first 3 instructions and then continues with the rest of the program. 
example5 :: Program
example5 = limited 3 (
            right 1 >*> forward 100 >*> color (RGB 255 0 0) >*> forward 100 >*> penup >*> right 1 >*> forward 100)  >*> left 2 >*> forward 50 >*> right 4

-- | This example shows the use of lifespan. It runs the first 3 instructions and then kills the turtle.
example6 :: Program
example6 = lifespan 3 (
            right 1 >*> forward 100 >*> color (RGB 255 0 0) >*> forward 100 >*> penup >*> right 1 >*> forward 100)  >*> left 2 >*> forward 50 >*> right 4

-- | Using Parallel composition
example7 :: Program 
example7 = ((forward 100 >*> right 1 >*> color (RGB 0 255 0) >*> backward 50) <|> (color (RGB 255 0 0) >*> forward 100 >*> right 2 >*> backward 200)) >*> right 1 >*> forward 100>*> left 2 >*> forward 50 >*> right 4

-- | A program that uses everything 
example8 :: Program
example8 = ((forward 100 >*> right 1 >*> color (RGB 0 255 0) >*> backward 50) <|> (color (RGB 255 0 0) >*> left 1 >*> times 3 (backward 60))) >*> lifespan 3 (right 1 >*> forward 100>*> left 2 >*> forward 50 >*> right 4)
