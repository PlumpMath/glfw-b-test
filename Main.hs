{-# LANGUAGE TemplateHaskell #-}
module Main where
import           Control.Lens
import           Control.Monad              (when)
import qualified Graphics.Rendering.OpenGL  as GL
import qualified Graphics.UI.GLFW           as GLFW
import           Reactive.Banana.Frameworks
import           System.IO

main :: IO ()
main = do
  initSuccess <- GLFW.init
  GLFW.setTime 0
  when initSuccess $ do
    window <- GLFW.createWindow 640 480 "GLFW-b Test" Nothing Nothing
    let testNetwork = do
          -- input : obtain Event from functions that register event
          -- handlers
          emouse    <- fromAddHandler $ GLFW.setKeyCallback window
          ekeyboard <- fromAddHandler $ GLFW.setMouseButtonCallback window
          reactimate $ fmap print emouse
          reactimate $ fmap print ekeyboard
    network <- compile testNetwork
    actuate network
