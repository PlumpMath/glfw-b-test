{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import           Control.Applicative
--import           Control.Lens
import           Control.Monad              (forever, when)
--import qualified Graphics.Rendering.OpenGL  as GL
import           Game.Handlers
import qualified Graphics.UI.GLFW           as GLFW
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           System.Exit                (exitSuccess)

main :: IO ()
main = do
  initSuccess <- GLFW.init
  GLFW.setTime 0
  when initSuccess $ do
    window <- GLFW.createWindow 640 480 "GLFW-b Test" Nothing Nothing
    case window of
      Just win -> mainLoop win
      Nothing  -> return ()

mainLoop :: GLFW.Window -> IO ()
mainLoop win = do
  let testNetwork :: forall t. Frameworks t => Moment t ()
      testNetwork = do
        -- input : obtain Event from functions that register event
        -- handlers
        eKeyChar <- fromAddHandler $ charHandler win
        eMouseButton <- fromAddHandler $ mouseButtonHandler win
        let
            eCharAccum = accumB [] $ (:) . snd <$> eKeyChar
        keyList <- changes eCharAccum
        eKey <- fromAddHandler $ keyHandler win
        reactimate $ const exitSuccess <$> eMouseButton
        reactimate $ print <$> eKeyChar
        reactimate $ print <$> keyList
        reactimate $ print <$> eKey
  network <- compile testNetwork
  actuate network
  putStrLn "Click the mouse button inside the window to exit!"
  forever GLFW.waitEvents

