{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where
import           Control.Applicative
--import           Control.Lens
import           Control.Monad              (forever, when)
import           Foreign.C.Types
import           Game.Handlers
import           Graphics.Rendering.OpenGL  (($=))
import qualified Graphics.Rendering.OpenGL  as GL
import qualified Graphics.UI.GLFW           as GLFW
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           System.Exit                (exitSuccess)
import           System.Random

newtype MyColor = MyColor { unMyColor :: GL.Color4 CFloat }

instance Random MyColor where
  randomR (MyColor (GL.Color4 rl gl bl al), MyColor (GL.Color4 rh gh bh ah)) gen = let
    (r,gen2) = randomR (rl,rh) gen
    (g,gen3) = randomR (gl,gh) gen2
    (b,gen4) = randomR (bl,bh) gen3
    (a,gen5) = randomR (al,ah) gen4
    in (MyColor (GL.Color4 r g b a),gen5)
  random gen = randomR (MyColor (GL.Color4 0.0 0.0 0.0 0.0), MyColor (GL.Color4 1.0 1.0 1.0 1.0)) gen

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
        eRandomColor <- changes =<< fromPoll (unMyColor <$> randomIO)
        let
            eCharAccum = accumB [] $ (:) . snd <$> eKeyChar
        keyList <- changes eCharAccum
        eKey <- fromAddHandler $ keyHandler win
        reactimate $ (GL.clearColor $=) <$> eRandomColor
        reactimate $ const exitSuccess <$> eMouseButton
        reactimate $ (\(_,c) -> if c == 'q' then exitSuccess else print c) <$> eKeyChar
        reactimate $ print <$> keyList
        reactimate $ print <$> eKey
  network <- compile testNetwork
  actuate network
  putStrLn "Click the mouse button inside the window to exit!"
  GLFW.makeContextCurrent $ Just win
  forever (draw >> GLFW.swapBuffers win >> GLFW.waitEvents)

draw :: IO ()
draw = do
  GL.clear [GL.ColorBuffer]
  GL.flush

