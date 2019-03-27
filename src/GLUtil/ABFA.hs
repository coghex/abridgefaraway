module GLUtil.ABFA where
-- all of the interfaces to graphics are abstracted
-- away to allow for new graphics libs (eg vulkan)
-- in the future.

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

type Window = GLFW.Window
type Error  = GLFW.Error
