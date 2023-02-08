{- |
Module      : Main
Description : From here you can run any program you want. You can choose an example from Example.hs 
or create your own set of actions. You can also modify the turtles initial state. 
-}
module Main where
import Turtle
import Examples 
import Control.Monad.Trans.State.Lazy
import Turtle (Instruction(StopAfter, DieAfter, Repeat))
import RunTurtle (runTurtle)
import qualified Graphics.HGL as HGL


-- p1 >*> p2 <|> p3 >> p4 



main :: IO()
main = do
  let initialTurtle = TT {pos = (250,250), angle = 0, pen =True, col = (RGB 255 255 255), time =0, parallel=False}
  -- let program = example8
  let program = [forward 100 >*> right 10,  color (RGB 255 0 0), forward 100, right 2, color (RGB 255 255 0), (forward 40 >*> right 1) <|>  (color (RGB 0 255 255) >*> backward 100), idle , forward 100 , color (RGB 255 0 0), forward 200]
  -- let program = [forward 100 >*> right 10,  color (RGB 255 0 0), forward 100, right 2, color (RGB 255 255 0), times 10 (forward 40 >*> right 1), color (RGB 0 255 255), idle , forward 100 , color (RGB 255 0 0), forward 200]
  -- let program = (<|>) ((>*>) (right 10) ((>*>) (color (RGB 0 255 0)) (forward 100))) ((>*>) (color (RGB 255 0 0)) (backward 300))
  -- let program = (right 1) >*> (color (RGB 0 255 0)  >*> forward 100) <|>  (color (RGB 255 0 0)  >*> backward 200)
  -- let program = forward 10 >*> right 45 >*> pendown >*>  (limited 3  ((forward 100) >*> backward 200 >*> color (RGB 0 0 255))) >*> (limited 3 (forward 100) >*> backward 200 >*> color (RGB 10 100 200)) >*> right 10 >*> idle >*> color (RGB 255 255 0) >*> backward 100
  _ <- runStateT (runTurtle program False Nothing) initialTurtle
  HGL.runGraphics $ do 
    HGL.withWindow_ "Turtle Graph" (500, 500) $ \w -> do
      _ <- runStateT (runTurtle program True (Just w)) initialTurtle
      -- HGL.drawInWindow w $ HGL.withRGB (HGL.RGB 0 255 0) $ HGL.line (255,255) (304,334)
      -- HGL.drawInWindow w $ HGL.withRGB (HGL.RGB 255 0 0) $ HGL.line (255,255) (50,255)
      HGL.getKey w
      HGL.closeWindow w
      putStrLn "Done"
  