{-# LANGUAGE RankNTypes, StandaloneDeriving, RecordWildCards, OverloadedStrings #-}
module Main where

import qualified Prelude
import Yage.Prelude

import Control.Monad (unless)
import qualified Yage as Y
import Yage.Core.Application
import Yage.Core.Application.Logging

import Data.List
import Control.Monad (mapM_)

import Linear
import Graphics.GLUtil.Camera3D (deg2rad)
import Graphics.GLUtil (getUniform)
import Yage.Types (YageState(..))
import Yage.Math
import Yage.Rendering
import Yage.Rendering.VertexSpec
import Yage.Rendering.Logging
import Yage.Rendering.Types
import Yage.Rendering.Primitives

import Graphics.Rendering.OpenGL.GL.VertexSpec
import             Graphics.Rendering.OpenGL.GL    (($=))
import             Graphics.Rendering.OpenGL.GL.Shaders.Uniform


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
    state <- Y.initialization

    let scene = testScene
        conf = ApplicationConfig DEBUG
        
    print $ show $ length $ entities scene
    execApplication "MainWireless" conf $ do
        win <- createWindowWithHints hints 800 600 "MainWireless Window-0"
        print <$> getWindowClientAPI win
        
        loop win scene (renderEnv state) (renderState state)

    Y.finalization state
    where 
        loop win scene env st = do
            let scene' = updateScene scene

            makeContextCurrent $ Just win
            (_, st', l) <- io $ runRenderer (renderScene scene') st env
            swapBuffers win
            
            unless (isEmptyRenderLog l) $ mapM_ debugM $ rlog'log l

            _ <- Y.processEvents
            quit <- windowShouldClose win
            --print $ show $ renderStatistics st
            unless quit $ loop win scene' env st'
        
        updateScene :: RenderScene -> RenderScene
        updateScene scene = 
            case (fromRenderable (head $ entities scene)) of
                (Just ent) -> 
                    let rot = axisAngle (signorm $ V3 1 1 1) (deg2rad 0.33)
                    in scene { entities = [SomeRenderable $ ent{ eOrientation = signorm $ (eOrientation ent) * rot }]
                             , sceneTime = 0.001 + sceneTime scene
                             }
                Nothing -> scene

testScene :: RenderScene
testScene = fill (emptyRenderScene)
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
                                    "textures"          != (0 :: GLint)
                                }
                rdef      = RenderDefinition
                                { def'ident    = "cube-base"
                                , def'data     = cubeMesh
                                , def'program  = (shader, shdef)
                                , def'textures = [("res/tex.png", 0)]
                                }
                box       = (mkRenderEntity rdef)
                                { eScale    = V3 2 2 2
                                , ePosition = V3 0 0 (-10)
                                }
            in scene{entities = [SomeRenderable box]}