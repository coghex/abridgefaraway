{-# LANGUAGE TemplateHaskell #-}
module Game.FontData where


import Graphics.Rendering.OpenGL
import Control.Lens

-- these data types are for the font renderer
type MatrixUpdate = [GLfloat] -> IO ()

data RndrProgram3D = RndrProgram3D { _program       :: Program
                                   , _setProjection :: MatrixUpdate
                                   , _setModelview  :: MatrixUpdate
                                   }
makeLenses ''RndrProgram3D

data TextRenderer = TextRenderer { _textProgram  :: RndrProgram3D
                                 , _setSampler   :: Index1 GLint   -> IO ()
                                 , _setTextColor :: Color4 GLfloat -> IO ()
                                 , _drawText     :: String -> IO ()
                                 }
makeLenses ''TextRenderer
