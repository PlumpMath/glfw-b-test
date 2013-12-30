{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import           Control.Applicative
--import           Control.Lens
import           Control.Monad              (forever, when)
--import qualified Graphics.Rendering.OpenGL  as GL
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
            eKeyAccum = accumB [] $ (:) . snd <$> eKeyChar
        keyList <- changes eKeyAccum
        reactimate $ const exitSuccess <$> eMouseButton
        reactimate $ print <$> eKeyChar
        reactimate $ print <$> keyList
  network <- compile testNetwork
  actuate network
  putStrLn "Click the mouse button inside the window to exit!"
  forever GLFW.waitEvents

charHandler :: GLFW.Window -> AddHandler (GLFW.Window, Char)
charHandler win callback = do
  GLFW.setCharCallback win . Just $ \w c -> callback (w, c)
  return (GLFW.setCharCallback win Nothing)

         
mouseButtonHandler :: GLFW.Window -> AddHandler ( GLFW.Window
                                                , GLFW.MouseButton
                                                , GLFW.MouseButtonState
                                                , GLFW.ModifierKeys)
mouseButtonHandler win callback = do
  GLFW.setMouseButtonCallback win . Just $ \w b bs mk -> callback (w, b, bs, mk)
  return (GLFW.setMouseButtonCallback win Nothing)
