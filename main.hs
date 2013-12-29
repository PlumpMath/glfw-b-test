{-# LANGUAGE TemplateHaskell #-}
module Main where
import           Control.Lens
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           Reactive.Banana
import           System.IO

main :: IO ()
main = do
  GLFW.init
  GLFW.setTime 0
  GLFW.createWindow 640 480 "GLFW-b Test" Nothing Nothing
  return ()

{-let networkDescription :: forall t Frameworks t = Moment t ()
    networkDescription = do
      -- input : obtain Event from functions that register event
      -- handlers
      emouse    <- fromAddHandler $ registerMouseEvent window
      ekeyboard <- fromAddHandler $ registerKeyEvent window
      -- input : obtain Behavior from mutable data by polling
      bdie      <- fromPoll       $ randomRIO (1,6)

      -- express event graph
-}
