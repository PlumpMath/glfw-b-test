{-# LANGUAGE TemplateHaskell #-}
module Main where
import           Control.Lens
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           Reactive.Banana
import           System.IO

main :: IO ()
main = do
  initSuccess <- GLFW.init
  GLFW.setTime 0
  window <- GLFW.createWindow 640 480 "GLFW-b Test" Nothing Nothing

  let testNetwork :: forall t. Frameworks t => Moment t ()
      testNetwork = do
         -- input : obtain Event from functions that register event
         -- handlers
         emouse    <- fromAddHandler $ setKeyCallback window
         ekeyboard <- fromAddHandler $ setMouseCallback window
      reactimate $ fmap print emouse
      reactimate $ fmap print ekeyboard

  network <- compile testNetwork
  actuate network
