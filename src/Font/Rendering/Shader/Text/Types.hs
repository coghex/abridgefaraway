{-# LANGUAGE TemplateHaskell #-}
module Font.Rendering.Shader.Text.Types where

import Graphics.Rendering.OpenGL
import Control.Lens
import Font.Rendering.Shader.Types


data TextShaderProgram = TextShaderProgram { _program       :: Program
                                           , _setProjection :: SetUniformMatrix4fv
                                           , _setModelview  :: SetUniformMatrix4fv
                                           , _setSampler    :: SetUniform1i
                                           , _setTextColor  :: SetUniformColor4f
                                           }
makeLenses ''TextShaderProgram

