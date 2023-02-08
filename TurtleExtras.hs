module TurtleExtras where 

import Turtle

-- | Draw square with the given size
square :: Double -> Turtle.Color -> Program
square d c = [pendown >*> color c >*> times 4 ( (>*>) (forward d) (right 1.5708))]


-- | Draw traingle with the given size
triangle :: Double -> Turtle.Color -> Program
triangle d c = [pendown >*> color c >*> times 3 ( (>*>) (forward d) (right 2.0944))]

-- | Draw octagon
octagon :: Double -> Turtle.Color -> Program
octagon d c = [pendown >*> color c >*> times 8 ( (>*>) (forward d) (right (pi / 4)))]

-- | Draw square with the given radius
circle :: Double -> Turtle.Color -> Program
circle r c = [pendown >*> color c >*> times 360 ( (>*>) (forward r) (right ((2/100)*pi)))]

