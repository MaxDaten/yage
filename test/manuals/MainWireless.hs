{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Prelude
import Yage.Prelude

import Control.Monad (mapM_, unless)
import Yage 

import Data.Typeable
import Data.List
import Data.Set hiding (map)

import Linear
import Graphics.GLUtil.Camera3D (deg2rad)
import Yage.Types (YageState(..))
import Yage.Math
import Yage.Events
import Yage.Rendering
import Yage.Rendering.VertexSpec
import Yage.Rendering.Logging
import Yage.Rendering.Types
import Yage.Rendering.Primitives

import Yage.Core.Application
import Yage.Core.Application.Loops
import Yage.Core.Application.Logging

hints :: [WindowHint]
hints = [ WindowHint'ContextVersionMajor  3
        , WindowHint'ContextVersionMinor  2
        , WindowHint'OpenGLProfile        OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat  True
        , WindowHint'RefreshRate          60
        , WindowHint'Resizable            False
        --, WindowHint'Decorated            False
        ]


class (Typeable i, Typeable a) => IsUpdateable i a where
    update :: i -> a -> a
    toUpdateable :: (Typeable u) => u -> Maybe a
    toUpdateable = cast


newtype Box = Box RenderEntity
    deriving (Typeable, Show)

instance Renderable Box where
    renderDefinition (Box b) = renderDefinition b

instance IsUpdateable InputState Box where
    update input (Box ent@RenderEntity{..}) =
        let rotV k a d = axisAngle a $ deg2rad (if input `isPressed` k then d else 0.0)
            rot        =  rotV Key'Right zAxis (-1.0)
                        * rotV Key'Left  zAxis   1.0
                        * rotV Key'Down  xAxis   1.0
                        * rotV Key'Up    xAxis (-1.0)
        in  Box ent{ eOrientation = signorm $ eOrientation * rot }


tryWithSomeRenderable :: (Typeable u, Renderable r) => (u -> r) -> SomeRenderable -> SomeRenderable
tryWithSomeRenderable f some = maybe some (toRenderable . f) (fromRenderable some)



main :: IO ()
main = 
    let scene = testScene
        conf  = defaultAppConfig{ logPriority = WARNING }
        size  = (800,600)
    in do
    state <- initialization

    (_, st, sc) <- execApplication "MainWireless" conf 
        $ basicWindowLoop size hints (renderEnv state, renderState state, scene) loop

    finalization state
    where 
    loop win (env, st, scene) inputState = do
        let scene' = scene `updateScene` inputState
        (_, st', l) <- io $ runRenderer (renderScene scene') st env
        
        return (env, st', scene')
            --unless (isEmptyRenderLog l) $ mapM_ debugM $ rlog'log l


updateScene :: RenderScene -> InputState -> RenderScene
updateScene scene input =
    let ents    = entities scene
        updateF = tryWithSomeRenderable (update input :: Box -> Box)
    in scene { entities = map updateF ents
             , sceneTime = 0.001 + sceneTime scene
             }



testScene :: RenderScene
testScene = fill emptyRenderScene
    where
        fill scene = 
            let shader    = ShaderResource "src/glsl/baseTex.vert" "src/glsl/baseTex.frag"
                shdef     = ShaderDefinition
                                { attrib'def = 
                                    [ "in_vert_position"  ^:= _position
                                    , "in_vert_normal"    ^:= _normal
                                    , "in_vert_color"     ^:= _color
                                    , "in_vert_texture"   ^:= _texture
                                    ]
                                , uniform'def = do
                                    ShaderEnv{..} <- shaderEnv
                                    -- io . print . show $ renderableType shaderEnv'CurrentRenderable
                                    -- io . print . show $ (fromRenderable shaderEnv'CurrentRenderable :: Maybe Box)
                                    let RenderScene{..} = shaderEnv'CurrentScene
                                        Just (Box RenderEntity{..}) = fromRenderable shaderEnv'CurrentRenderable
                                        scaleM        = kronecker . point $ eScale
                                        projectionM   = projectionMatrix
                                        viewM         = viewMatrix
                                        transM        = mkTransformation eOrientation ePosition
                                        modelM        = transM !*! scaleM 
                                        Just normalM  = adjoint <$> (inv33 . fromTransformation $ modelM)
                                    "projection_matrix" != projectionM
                                    "view_matrix"       != viewM
                                    "model_matrix"      != modelM
                                    "normal_matrix"     != normalM
                                }
                rdef      = RenderDefinition
                                { def'ident    = "cube-base"
                                , def'data     = cubeMesh
                                , def'program  = (shader, shdef)
                                , def'textures = [TextureDefinition (0, "textures") ("res" </> "Brown_Leather_Texture.png")]
                                }
                box       = Box (mkRenderEntity rdef)
                                { eScale    = V3 2 2 2
                                , ePosition = V3 0 0 (-10)
                                }
            in scene{entities = [SomeRenderable box]}

