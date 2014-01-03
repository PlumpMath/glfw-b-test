{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where
--import           Control.Lens
import           qualified Control.Monad as M
import           Foreign.Ptr
import           FRP.Netwire
import           Game.Handlers
import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import Prelude hiding ((.),id)
import           Paths_glfw_b_test
import           System.Exit               (exitSuccess)
import           System.Random

main :: IO ()
main = withInitGLFW $ \win -> do
  prog <- initializeOpenGL
  GL.currentProgram $= Just (program prog)
  GLFW.setKeyCallback win (Just kcb)
  M.forever $ do
    parseEvents
    draw
    GLFW.swapBuffers win
  where
    kcb _ _ _ _ _ = putStrLn "kcb!"
    parseEvents = do
      GLFW.pollEvents

withInitGLFW :: (GLFW.Window -> IO ()) -> IO ()
withInitGLFW act = GLFW.init >>= \i -> M.when i $ do
  win <- GLFW.createWindow 640 480 "GLFW-b Test" Nothing Nothing
  GLFW.makeContextCurrent win
  maybe (return ()) act win

initializeOpenGL :: IO ShaderProgram
initializeOpenGL = do
  (vpath:fpath:_) <- mapM getDataFileName ["shaders/Simple.vert", "shaders/Simple.frag"]
  prog <- simpleShaderProgram vpath fpath
  print $ attribs prog
  buf <- makeBuffer GL.ArrayBuffer triangle
  enableAttrib prog "attribPosition"
  setAttrib prog "attribPosition" GL.ToFloat (GL.VertexArrayDescriptor 4 GL.Float 0 nullPtr)
  return prog
  where triangle :: [Float]
        triangle  = [ 0.0, 1.0, 0.0, 1.0
                    , 1.0,-1.0, 0.0, 1.0
                    ,-1.0,-1.0, 0.0, 1.0
                    ]

draw :: IO ()
draw = do
  GL.drawArrays GL.Triangles 0 3
  GL.flush
