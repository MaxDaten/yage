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
import Control.Lens ((&))

import Linear
import Graphics.GLUtil.Camera3D (deg2rad)
import Graphics.GLUtil (setUniform, getUniform, asUniform)
import Yage.Types (YageState(..))
import Yage.Math
import Yage.Rendering
import Yage.Rendering.VertexSpec
import Yage.Rendering.Logging
import Yage.Rendering.Types
import Yage.Rendering.Primitives

hints = [ WindowHint'ContextVersionMajor 3
        , WindowHint'ContextVersionMinor 2
        , WindowHint'OpenGLProfile OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat True]

main :: IO ()
main = do
    state <- Y.initialization

    let scene = testScene
        conf = ApplicationConfig DEBUG
        
    print $ show $ length $ entities scene
    execApplication "MainWireless" conf $ do
        win <- createWindowWithHints hints 800 600 "MainWireless Window-0"
        makeContextCurrent $ Just win
        print <$> getWindowClientAPI win
        
        loop win scene (renderEnv state) (renderState state)

    Y.finalization state
    where 
        loop win scene env st = do
            _ <- Y.processEvents
            let scene' = updateScene scene

            swapBuffers win
            (_, st', l) <- io $ runRenderer (renderScene scene') st env
            unless (isEmptyRenderLog l) $ mapM_ debugM $ rlog'log l

            quit <- windowShouldClose win
            --print $ show $ renderStatistics st
            unless quit $ loop win scene' env st'
        
        updateScene :: RenderScene -> RenderScene
        updateScene scene = case (fromRenderable (head $ entities scene)) of
            (Just ent) -> 
                let rot = axisAngle (signorm $ V3 1 1 1) (deg2rad 0.5)
                in scene { entities = [SomeRenderable $ ent{ eOrientation = signorm $ (eOrientation ent) * rot}]
                         , sceneTime = 0.001 + sceneTime scene
                         }
            Nothing -> scene

testScene :: RenderScene
testScene = fill (emptyRenderScene)
    where
        fill scene = 
            let shader    = ShaderResource "src/glsl/base.vert" "src/glsl/base.frag"
                shdef     = ShaderDefinition
                                { attrib'def = 
                                    [ "in_vert_position"  ^:= _position
                                    , "in_vert_normal"    ^:= _normal
                                    , "in_vert_color"     ^:= _color
                                    ]
                                , uniform'def = ShaderUniformDef $ \r RenderScene{..} p -> do
                                    let Just ent = fromRenderable r :: Maybe RenderEntity
                                        projectionM   = projectionMatrix
                                        viewM         = viewMatrix
                                        modelM        = mkTransformation (eOrientation ent) (ePosition ent)
                                        Just normalM  = inv33 . fromTransformation $ modelM
                                    getUniform p "projection_matrix" & asUniform projectionM
                                    getUniform p "view_matrix"       & asUniform viewM
                                    getUniform p "model_matrix"      & asUniform modelM
                                    getUniform p "normal_matrix"     & asUniform normalM
                                }
                rdef      = RenderDefinition
                                { def'ident   = "cube-base"
                                , def'data    = cubeMesh
                                , def'program = (shader, shdef)
                                }
                box       = (mkRenderEntity rdef)
                                { eScale    = V3 1 1 1
                                , ePosition = V3 0 0 (-20)
                                }
            in scene{entities = [SomeRenderable box]}