{- |
Module      : Turtle
Description : Implementing abstact data type Instruction and Program. All the possible actions a turtle can do are defined. 
We define a data type called TT which determines a turtle. This keeps track of turtle position, angle, pen color....

Note: rotations are meassured in radians and not degrees. 

This module implements an embedded language for turtle graphics. It defines abstract data types 
  - 'Program' which defines the movements a turtle can perform
  - 'Action' it records the actions the turtle does in the shape of a line and a string 
  - 'Line' records the data necessary to draw a line
  - 'Turtle' represents a turtle state. It contains the attributes of a turtle state.

It also contains run functions: 
  - 'runTurtle' takes in a turtle state and a program and returns a list of actions that we will use to print either graphical or textual interface
  - 'runGraphical' carries out turtle actions in graphical form
  - 'runTextual' prints out information about the actions performed bby turtle

Note: rotations are meassured in radians and not degrees. Each action is considered a unit of time
-}

module Turtle (
  -- | Data types
  Program,
  Turtles(..), 
  Color(..),

  -- | Primitive operations
  forward, 
  right, 
  penup, 
  pendown, 
  color, 
  die, 
  (>*>),
  (<|>),
  times, 
  
  -- | Derived operations
  backward, 
  left, 
  forever, 
  idle, 
  limited, 
  lifespan, 

  -- | Run functions
  runTextual, 
  runGraphical
) where
import qualified Graphics.HGL as HGL
import Data.Word

-- | Describes the possible actions a turtle can perform
data Program  = 
    Move Double 
  | Rotate Double 
  | PenUp
  | PenDown
  | Color Color
  | Die
  | Sequence Program Program
  | Parallel Program Program
  deriving (Show,Eq)

-- | Describes information to later represent graphically (line) or textually (output)
data Action = Action
    { line :: Line
    , output :: String
    } deriving Show 

-- | Describes attributes necesarry to represent a graphical line
data Line = Line {sPos :: Point
                , ePos :: Point
                , lcol  :: Color
                , lpen :: Bool
                } deriving Show

-- | Represents a point
type Point = (Int,Int)

-- | Represents a color
data Color = RGB Word8 Word8 Word8 
  deriving (Show,Eq)

-- | Describes a turtle. It defines the necessary attributes to carry out all the actions
data Turtles = Turtles { pos :: Point
                , angle :: Double
                , pen :: Bool
                , col :: Color
                , time :: Int   
                , dead :: Bool
 }  deriving Show 

-- | PRIMITIVE OPERATIONS
-- | Move turtle forward n steps
forward :: Double -> Program 
forward n = Move n

-- | Rotate turtle right d degrees
right :: Double -> Program 
right d = Rotate d

-- | Puts pen up (turtle stops drawing)
penup ::  Program
penup = PenUp

-- | Puts pen down (turtle starts drawing)
pendown :: Program
pendown = PenDown

-- | Changes the color of the turtle's pen
color :: Color -> Program 
color (RGB r g b) = Color (RGB r g b)

-- | Kills the turtle 
die :: Program
die = Die

-- | Allows to run two programs sequentially
-- infixl 7 >*>
(>*>) :: Program -> Program -> Program
p1 >*> p2 = Sequence p1 p2 

-- | Allows turtles to run in parallel
-- infixl 6 <|>
(<|>) :: Program -> Program -> Program
p1 <|> p2 = Parallel p1 p2

-- | DERIVED OPERATIONS
-- | Move turtle backwards n steps
backward :: Double -> Program 
backward n = forward (negate n)

-- | Rotate turtle left d degrees 
left :: Double -> Program 
left d = right (negate d)

-- | Turtle repeats a program p infinite times
forever :: Program -> Program
forever p = p >*> (forever p)

-- | Represents a program that does nothing
idle :: Program 
idle = forward 0

-- | Repeats program p n times
times :: Int -> Program -> Program
times 1  p = p
times n  p = Sequence p (times (n-1) p)

-- | Makes turtle stop program p after n units of time 
limited :: Int -> Program -> Program 
limited n p = limit n prog
 where prog = flatten p []
       limit 1 (p:ps) = p 
       limit n (p:ps) = p >*> limit (n-1) ps 

-- | Performs program p for n units of time and then kills turtle
lifespan :: Int -> Program -> Program  
lifespan n p = limit n prog
 where prog = flatten p []
       limit 1 (p:ps) = p >*> die
       limit n (p:ps) = p >*> limit (n-1) ps 

-- | Helper function that flattens program to implement limited and lifespan
flatten :: Program -> [Program] -> [Program]
flatten (Sequence p1 p2) acc = flatten p1 $ flatten p2 acc
flatten p acc = p : acc

-- | RUN FUNCTIONS
-- | runTurtle takes in an initial turtle and a program and performs returns the performed actions
runTurtle :: Turtles -> Program -> [Action] -> ([Action], Turtles)
runTurtle turtle p a = case (dead turtle) of 
    True -> (a,turtle)
    False -> case p of 
      (Move s) -> let 
        (currentPos, newPos) = getNewPos (pos turtle) s (angle turtle)
        updTurtle = turtle{pos=newPos, time = time turtle + 1}
        str = "At time "++ show (time turtle) ++ " turtle moves from " ++ show (pos turtle) ++ " to " ++ show newPos
        newLine = Line {sPos = currentPos, ePos = newPos, lcol = col turtle, lpen = pen turtle}
        action  = Action {line = newLine, output=str }  
        in ([action], updTurtle)

      (Rotate d) -> let 
        updTurtle = turtle {angle = (angle turtle) +d, time = time turtle + 1}
        str = "At time " ++ show (time turtle) ++ " turtle rotates " ++ show d ++ " degrees" 
        -- action = Action {output=str }  
        l = Line{ePos=(pos turtle), sPos= (pos turtle), lcol = (col turtle), lpen = pen turtle } 
        action = Action {line = l, output=str }
        in ([action], updTurtle)

      PenUp -> let 
        updTurtle = turtle {pen = False , time = time turtle + 1}
        str = "At time " ++ show (time turtle) ++ " turtle puts pen up"
        -- action = Action {output=str }  
        l = Line{ePos=(pos turtle), sPos= (pos turtle), lcol = (col turtle),lpen = pen turtle } 
        action = Action {line = l, output=str }
        in ([action], updTurtle)

      PenDown -> let 
        updTurtle = turtle {pen = True, time = time turtle + 1}
        str = "At time " ++ show (time turtle) ++ " turtle puts pen down"
        -- action = Action {output=str }  
        l = Line{ePos=(pos turtle), sPos= (pos turtle), lcol = (col turtle), lpen = pen turtle } 
        action = Action {line = l, output=str }
        in ([action], updTurtle)

      (Color (RGB r g b)) ->  let 
        updTurtle = turtle {col=(RGB r g b), time = time turtle + 1}
        str = "At time " ++ show (time turtle) ++ " turtle changes pen color to " ++ show r ++ " " ++ show g ++ " " ++ show b
        -- action = Action {output=str } 
        l = Line{ePos=(pos turtle), sPos= (pos turtle), lcol = (col turtle), lpen = pen turtle } 
        action = Action {line = l, output=str }
        in ([action], updTurtle)

      Die -> let 
        updTurtle = turtle {dead= True, time = time turtle + 1}
        str = "At time " ++ show (time turtle) ++ " turtle has died :("
        -- action = Action {output=str}
        l = Line{ePos=(pos turtle), sPos= (pos turtle), lcol = (col turtle),lpen = pen turtle } 
        action = Action {line = l, output=str }
        in ([action], updTurtle)

      (Sequence p1 p2) -> let
        (as1, t')  = runTurtle turtle p1 a 
        (as2, t'') = runTurtle t' p2 a
        in (as1 ++ as2, t'') 
      
      (Parallel p1 p2) -> let 
        (as1, t') = runTurtle turtle p1 a 
        (as2, t'') = runTurtle turtle p2 a 
        in (myZip as1 as2, turtle)

-- | Helper function. Used to run parallel turtles
myZip :: [a] -> [a] -> [a]
myZip [] []          = []
myZip [] xs          = xs
myZip ys []          = ys
myZip (x:xs) (y:ys)  = x : y : myZip xs ys

-- | Takes in the current turtle position and calculates the position the turtle will be after moving (it considers the angle the turtle is at). 
getNewPos :: HGL.Point -> Double -> Double -> (HGL.Point , HGL.Point)
getNewPos pos x angle = let -- angleRadians = angle * pi / 180.0
                            newX = fromIntegral (fst pos) + x * cos angle    -- ^ The turtle is moved x units forward in the direction of its current angle, and a line is drawn to trace its movement.
                            newY = fromIntegral (snd pos) + x * sin angle    -- ^ Turtle's angle is used to determine the direction in which it should move
                            newPos = (round newX, round newY)
                        in (pos,newPos)

-- | Gets a Color and turns it into a  HGL.Color 
toHglColor :: Color -> HGL.RGB
toHglColor (RGB r g b) = HGL.RGB r g b 

-- | Returns the actions carried by a turtle in graphical form
runGraphical :: Turtles -> Program ->  IO ()
runGraphical t p = let actions = fst $ runTurtle t p []
    in  HGL.runGraphics $ do
        w <- HGL.openWindowEx "Turtle!" Nothing (500, 500) HGL.DoubleBuffered (Just 1000)
        onTick w $ runGraphical' actions
        HGL.getKey w >> return ()

-- | Helper function for runGraphical
onTick :: HGL.Window -> [HGL.Graphic] -> IO ()
onTick w []      = return ()
onTick w (x:xs)  = do
  HGL.getWindowTick w
  HGL.drawInWindow w x
  onTick w xs

-- | Helper function for runGraphical
runGraphical' :: [Action] -> [HGL.Graphic]
runGraphical' xs = map (\action -> if (lpen (line action)) then HGL.withRGB (toHglColor (lcol (line action))) $ HGL.line (sPos (line action)) (ePos (line action)) 
                                   else return() )xs

-- | Helper function for runTextual
runTextual' :: [Action] -> IO ()
runTextual' xs = mapM_ (putStrLn . output) xs


-- | Returns a textual representation of the turtles actions performed
runTextual :: Turtles -> Program -> IO ()
runTextual turtle p = runTextual' $ fst $ runTurtle turtle p []