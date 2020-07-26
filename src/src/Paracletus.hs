{-# LANGUAGE Strict #-}
module Paracletus where
-- a graphics layer is chosen
-- and glfw instance is begun
import Prelude()
import UPrelude
import Anamnesis
import Anamnesis.Util
import Artos.Except
import Paracletus.Data
import Paracletus.Vulkan

runParacletus ∷ GraphicsLayer → Anamnesis ε σ ()
runParacletus Vulkan   = runParacVulkan
runParacletus OpenGL   = logExcept ParacError ExParacletus $ "OpenGL not yet implimented"
runParacletus OpenGLES = logExcept ParacError ExParacletus $ "OpenGLES not yet implimented"
runParacletus _        = logExcept ParacError ExParacletus $ "unsupported graphics layer..."
