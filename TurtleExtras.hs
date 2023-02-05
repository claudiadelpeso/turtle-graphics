module TurtleExtras where 

import Turtle
import Graphics.HGL 

-- | Draw square with the given size
square :: Double -> Turtle.Color -> Program
square d c = [pendown >*> color c >*> times 4 ( (>*>) (forward d) (right 1.5708))]


-- | Draw traingle with the given size
triangle :: Double -> Turtle.Color  -> Program
triangle d c = [pendown >*> color c >*> times 3 ( (>*>) (forward d) (right 2.0944))]

-- | Draw octagon
octagon :: Double -> Turtle.Color  -> Program
octagon d c = [pendown >*> color c >*> times 8 ( (>*>) (forward d) (right (pi / 4)))]

-- | Draw square with the given radius
circle :: Double -> Turtle.Color  -> Program
circle r c = [pendown >*> color c >*> times 360 ( (>*>) (forward r) (right ((2/100)*pi)))]

-- | Finite spiral: stops at 100 steps and turns with angle "angle" to the right
spiral :: Double -> Double -> Instruction 
spiral size angle | size > 100 = die 
                  | otherwise = do 
                    forward size >*> right angle >*> spiral (size+2) angle

-- | Infinite spiral that rotates right with angle "angle"
infiniteSpiral :: Double -> Double -> Instruction 
infiniteSpiral size angle = 
    forward size >*> right angle >*> infiniteSpiral size angle


-- | Can you define the limited version in terms of the unlimited one? Yes
limitedV :: Double -> Double -> Int -> Instruction 
limitedV size angle int  = lifespan int [infiniteSpiral size angle]

{- |
the limited function takes an integer "time" and "program" as input and returns
a new program that runs for the specified amount of time. The 'lifespan' function is used
to limit the lifespan of the turtle. The program is then composed with an idle command, which does nothing
and an original program, so the turtle will perform nothing for the remaining time and stop 
-} 

-- | Finite spiral and infinite spiral where the finite spiral ended 
twoSpirals :: Double -> Double -> Instruction 
twoSpirals size angle | size > 100 = infiniteSpiral size angle 
                      | otherwise = do 
                         forward size >*> right angle >*> spiral (size+2) angle