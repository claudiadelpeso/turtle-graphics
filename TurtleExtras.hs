module TurtleExtras where 

import Turtle 
import Graphics.HGL 

-- | Draw square with the given size
square :: Double -> Turtle.Color -> [Instruction]
square d c = [pendown >*> color c >*> times 4 ( (>*>) (forward d) (right 1.5708))]


-- | Draw traingle with the given size
triangle :: Double -> Turtle.Color -> [Instruction]
triangle d c = [pendown >*> color c >*> times 3 ( (>*>) (forward d) (right 2.0944))]

-- | Draw octagon
octagon :: Double -> Turtle.Color -> [Instruction]
octagon d c = [pendown >*> color c >*> times 8 ( (>*>) (forward d) (right (pi / 4)))]

-- | Draw square with the given radius
circle :: Double -> Turtle.Color -> [Instruction]
circle r c = [pendown >*> color c >*> times 360 ( (>*>) (forward r) (right ((2/100)*pi)))]


