{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Prelude
import Yage.Prelude

import Control.Monad (mapM_, unless)
import Yage 

import Data.List

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

main :: IO ()
main = do
    state <- initialization

    let scene = testScene
        conf = ApplicationConfig DEBUG
        
    print $ show $ length $ entities scene
    execApplication "MainWireless" conf $ do
        win <- createWindowWithHints hints 800 600 "MainWireless Window-0"
        print <$> getWindowClientAPI win
        
        loop win scene (renderEnv state) (renderState state)

    finalization state
    where 
        loop win scene env st = do
            makeContextCurrent $ Just win
            (_, st', l) <- io $ runRenderer (renderScene scene) st env
            swapBuffers win
            
            unless (isEmptyRenderLog l) $ mapM_ debugM $ rlog'log l

            events <- collectEvents
            quit <- windowShouldClose win

            let scene' = updateScene scene events
            unless quit $ loop win scene' env st'
        
        updateScene :: RenderScene -> Set Event -> RenderScene
        updateScene scene events = 
            let Just ent = fromRenderable $ head $ entities scene
                rot      = axisAngle (signorm $ V3 0 1 0) $ deg2rad (if keyPressed Key'Right events then 1.0 else 0)
            in scene { entities = [SomeRenderable $ ent{ eOrientation = signorm $ eOrientation ent * rot }]
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
                                    let RenderScene{..} = shaderEnv'CurrentScene
                                        Just RenderEntity{..} = fromRenderable shaderEnv'CurrentRenderable :: Maybe RenderEntity
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
                                , def'textures = [TextureDefinition (0, "textures") "res/tex.png"]
                                }
                box       = (mkRenderEntity rdef)
                                { eScale    = V3 2 2 2
                                , ePosition = V3 0 0 (-10)
                                }
            in scene{entities = [SomeRenderable box]}