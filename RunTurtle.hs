
{- | 
Module : RunTurtle 
Description : This module contains the run function. It considers each possible action and gives the appropriate instructions. It uses the stateful monad to keep track of the current turtle state. 
The function takes a boolean argument that indicates if we want to run the graphical interface (True) or the text interface (False). The graphical option draws the actions the turtle performs on a new HGL window. 
The text option prints to screen the actions the turtle is performing, at what time and other additional information. Here each instruction is considered to take one time unit of time.
It includes functionality for infinite programs. However, if running both at the same time, it will only print to screen and not draw the actions. This is because the text function never ends so the graphical one never starts. 
-}
module RunTurtle where 
import Turtle 
import qualified Graphics.HGL as HGL
import Control.Monad as M
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Data.Maybe

-- | Gets a Color and turns it into a  HGL.Color 
toHglColor :: Color -> HGL.RGB
toHglColor (RGB r g b) = HGL.RGB r g b 

-- | Takes in the current turtle position and calculates the position the turtle will be after moving (it considers the angle the turtle is at). 
getNewPos :: HGL.Point -> Double -> Double -> (HGL.Point , HGL.Point)
getNewPos pos x angle = let newX = fromIntegral (fst pos) + x * cos angle    -- ^ The turtle is moved x units forward in the direction of its current angle, and a line is drawn to trace its movement.
                            newY = fromIntegral (snd pos) + x * sin angle    -- ^ Turtle's angle is used to determine the direction in which it should move
                            newPos = (round newX, round newY)
                        in (pos,newPos)

-- | It takes a list of instructions and carries them out one by one
runTurtle :: [Instruction] -> Bool ->  Maybe HGL.Window -> StateT TT IO()
-- | When there are no more instructions left
runTurtle [] b w = do 
                      case w of 
                        Nothing -> return()
                        Just e -> do   
                              lift $ HGL.getKey e
                              lift $ HGL.closeWindow e

runTurtle ((Move s):ps) b w = do 
    turtle <- get 
    let (currentPos, newPos) = getNewPos (pos turtle) s (angle turtle)
    case b of 
      True ->  when (pen turtle) $ lift $ HGL.drawInWindow (fromJust w) $ HGL.withRGB (toHglColor (col turtle)) $ HGL.line currentPos newPos    -- ^ draw line from pos to new position
      False -> lift $ putStrLn $ "At time "++ show (time turtle) ++ " turtle moves from " ++ show (pos turtle) ++ " to "  ++ show newPos  
    put $ TT{pos = newPos, angle = angle turtle, pen = pen turtle, col = col turtle, time = (time turtle)+1} -- ^ update position and time
    runTurtle ps b w 

runTurtle ((Rotate d):ps) b w = do 
      turtle <- get 
      case b of 
            True -> return()-- do nothing
            False -> lift $ putStrLn $ "At time " ++ show (time turtle) ++ " turtle rotates " ++ show d ++ " degrees" 
      put $ TT{pos = pos turtle, angle = (angle turtle)+d, pen = pen turtle, col = col turtle, time = (time turtle)+1}
      runTurtle ps b w 

runTurtle (Stop:ps) b w = do 
      turtle <- get 
      case b of 
            True -> return()-- do nothing
            False -> lift $ putStrLn $ "At time " ++ show (time turtle) ++ " turtle puts pen up "
      put $ TT{pos = pos turtle, angle = angle turtle, pen = False, col = col turtle, time = (time turtle)+1}
      runTurtle ps b w   
runTurtle (Start:ps) b w = do
      turtle <- get 
      case b of 
            True -> return()-- do nothing
            False -> lift $ putStrLn $ "At time " ++ show (time turtle) ++ " turtle puts pen down "
      put $ TT{pos = pos turtle, angle = angle turtle, pen = True, col = col turtle, time = (time turtle)+1}
      runTurtle ps b w  


runTurtle ((C (RGB r g bb)):ps) b w = do
      turtle <- get 
      case b of 
            True -> return()-- do nothing
            False -> lift $ putStrLn $ "At time " ++ show (time turtle) ++ " turtle changes pen color to " ++ show r ++ " " ++ show g ++ " " ++ show bb
      put $ TT{pos = pos turtle, angle = angle turtle, pen = pen turtle, col = (RGB r g bb), time = (time turtle)+1}
      runTurtle ps b w 

runTurtle (Die:_) b w = do 
      turtle <- get
      case b of 
            True -> case w of 
                  Nothing -> return()
                  _ -> do 
                        lift $ HGL.getKey (fromJust w)
                        lift $ HGL.closeWindow (fromJust w)
            False -> lift $ putStrLn $ "At time " ++ show (time turtle) ++ " turtle has died :("
          
runTurtle (Idle:ps) b w = do 
          turtle <- get 
          case b of 
                True -> return()-- do nothing
                False -> lift $ putStrLn $ "At time " ++ show (time turtle) ++ " turtle does nothing"
          put $ TT{pos = pos turtle, angle = angle turtle, pen = pen turtle, col = col turtle, time = (time turtle)+1}
          runTurtle ps b w 

runTurtle ((StopAfter t inst):ps) b w = do 
          turtle <- get 
          runTurtle ((take t inst)++ps) b w
      
runTurtle ((DieAfter t inst):ps) b w =  do 
          turtle <- get 
          runTurtle (take t inst) b w
          runTurtle [] b w 

runTurtle ((Repeat n inst):ps) b w = do 
      turtle <- get 
      runTurtle ((replicate n inst) ++ ps) b w


runTurtle ((Sequence p1 p2):ps) b w = do 
          turtle <- get 
          runTurtle (p1:p2:ps) b w 

-- | Do not know how to implement this so that two different things get drawn
runTurtle ((Parallel p1 p2):ps) b w = do 
           runTurtle [p1] b w 
           runTurtle [p2] b w 
           runTurtle ps b w
