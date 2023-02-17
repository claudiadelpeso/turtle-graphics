{- |
Module      : SpinTurtle
Description : This module creates different spirals. 
-}
module SpinTurtle where 

import Turtle (Program, forward, right, die, lifespan, (>*>))

-- | Finite spiral: stops at 100 steps and turns with angle "angle" to the right
spiral :: Double -> Double -> Program 
spiral size angle | size > 100 = die
                  | otherwise = do 
                    forward size >*> right angle >*> spiral (size+2) angle

-- | Infinite spiral that rotates right with angle "angle"
infiniteSpiral :: Double -> Double -> Program
infiniteSpiral size angle = 
    forward size >*> right angle >*> infiniteSpiral (size+2) angle


-- | Can you define the limited version in terms of the unlimited one? Yes
limitedV :: Double -> Double -> Int -> Program
limitedV size angle int  = lifespan int (infiniteSpiral size angle)

{- |
the limited function takes an integer "time" and "program" as input and returns
a new program that runs for the specified amount of time. The 'lifespan' function is used
to limit the lifespan of the turtle. The program is then composed with an idle command, which does nothing
and an original program, so the turtle will perform nothing for the remaining time and stop 
-} 

-- | Finite spiral and infinite spiral where the finite spiral ended 
twoSpirals :: Double -> Double -> Program 
twoSpirals size angle | size > 100 = infiniteSpiral size angle 
                      | otherwise = do 
                         forward size >*> right angle >*> spiral (size+2) angle
