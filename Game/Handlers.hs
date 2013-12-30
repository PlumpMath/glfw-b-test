module Game.Handlers where
import qualified Graphics.UI.GLFW           as GLFW
import           Reactive.Banana.Frameworks

charHandler :: GLFW.Window -> AddHandler (GLFW.Window, Char)
charHandler win callback = do
  GLFW.setCharCallback win . Just $ \w c -> callback (w, c)
  return (GLFW.setCharCallback win Nothing)

keyHandler :: GLFW.Window -> AddHandler ( GLFW.Window
                                        , GLFW.Key
                                        , Int
                                        , GLFW.KeyState
                                        , GLFW.ModifierKeys)
keyHandler win callback = do
  GLFW.setKeyCallback win . Just $ \w k num ks mk -> callback (w, k, num, ks, mk)
  return (GLFW.setKeyCallback win Nothing)

mouseButtonHandler :: GLFW.Window -> AddHandler ( GLFW.Window
                                                , GLFW.MouseButton
                                                , GLFW.MouseButtonState
                                                , GLFW.ModifierKeys)
mouseButtonHandler win callback = do
  GLFW.setMouseButtonCallback win . Just $ \w b bs mk -> callback (w, b, bs, mk)
  return (GLFW.setMouseButtonCallback win Nothing)
