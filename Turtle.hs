{- |
Module      : Turtle
Description : Implementing abstact data type Instruction and Program. All the possible actions a turtle can do are defined. 
We define a data type called TT which determines a turtle. This keeps track of turtle position, angle, pen color....

Note: rotations are meassured in radians and not degrees. 
-}

module Turtle (
-- Abstract datatype and other data types for turtle program 
Instruction(..),
Color (RGB),
TT(..), 
Point,

  -- Movement commands
forward, 
backward, 
right, 
left, 

  -- Drawing commands
penup, 
pendown,
color,

  -- State commands
die, 
idle, 
limited, 
lifespan, 

 -- Repeating commands 
times, 
forever, 

 -- Sequencing operator 
(>*>),
(<|>)
) where

import GHC.Types()
import Data.Word

-- | Instruction data type: It describes all the possible actions that the turtle can do.
-- we then implement functions that use this constructors to do actions.
data Instruction =  Move Double | Rotate Double | Stop | Start | C Color |
               Die | Idle | StopAfter Int Program | DieAfter Int Program | 
               Repeat Int Instruction | Sequence Instruction Instruction | Parallel Instruction Instruction

-- | Program data type: It holds a list of instructions. 
-- We implemented like this because a turtle will want to do several actions together not just one. 
type Program = [Instruction]

-- | Point data type
type Point = (Int, Int)

-- | Data to represet a turtle
data TT = TT {
  pos    :: Point,
  angle  :: Double, 
  pen    :: Bool, 
  col    :: Color, 
  time   :: Int, 
  parallel :: Bool
} 
-- | PRIMITIVE OPERATIONS
-- | move forward the gven number of steps
forward :: Double -> Instruction 
forward = Move

-- | turn right the given number of degrees
right :: Double -> Instruction 
right = Rotate

-- | stop drawing
penup :: Instruction 
penup = Stop 

-- | start drawing
pendown :: Instruction 
pendown = Start

-- | The 'Color' data type represents a color that can be used to draw on the screen.
data Color = RGB Word8 Word8 Word8

color :: Color -> Instruction 
color (RGB r g b)= C (RGB r g b)

-- |"kill" the turtle (unable to perform any more actions)
die :: Instruction 
die = Die 

-- | makes the turtle stop what it is doing after a specified period of time (t)
limited :: Int -> Program -> Instruction 
limited  = StopAfter

-- | kills the turtle program after a specified period of time (t)
lifespan :: Int -> Program -> Instruction 
lifespan = DieAfter 


-- | repeat a program (prog) a (int) number of times
times :: Int -> Instruction -> Instruction 
times = Repeat 

-- | Sequencing operator: it performs instrutions/commands one after another

infixl 7 >*>
(>*>) :: Instruction -> Instruction -> Instruction 
(>*>) = Sequence

-- | Parallel composition combinator 
infixl 6 <|>
(<|>) :: Instruction -> Instruction -> Instruction 
(<|>) = Parallel 

-- | DERIVED OPERATIONS

-- | move backward the given number of steps 
backward :: Double -> Instruction 
backward steps = forward (negate steps)

-- | turn left the given number of degrees 
left :: Double -> Instruction 
left deg = right (negate deg)

-- | repeats a program forever (derived)
forever :: Instruction -> Instruction 
forever p = (>*>) p (forever p)

-- | a program that does nothing
idle :: Instruction 
idle = forward 0

