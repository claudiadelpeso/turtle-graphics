{- |
Module      : Main
Description : From here you can run any program you want. You can choose an example from Example.hs 
or create your own set of actions. You can also modify the turtles initial state. 
-}
module Main where
import Turtle
import Examples (example8)
import Control.Monad.Trans.State.Lazy
import Turtle (Instruction(StopAfter, DieAfter, Repeat))
import RunTurtle (runTurtle)
import qualified Graphics.HGL as HGL


main :: IO()
main = do
  let initialTurtle = TT {pos = (250,250), angle = 0, pen =True, col = (RGB 255 255 255), time =0 }
  let program = example8
  _ <- runStateT (runTurtle program False Nothing) initialTurtle 
  HGL.runGraphics $ do 
    HGL.withWindow_ "Turtle Graph" (500, 500) $ \w -> do
      _ <- runStateT (runTurtle program True (Just w)) initialTurtle
      putStrLn "Done"
  