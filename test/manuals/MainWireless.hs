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

import Control.Lens

import Linear
import Graphics.GLUtil.Camera3D (deg2rad)
import Yage.Types (YageState(..))
import Yage.Math
import Yage.Events
import Yage.Font
import Yage.Texture.Atlas
import Yage.Rendering
import Yage.Rendering.Texture
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


fontchars = " !\"#$%&'()*+,-./0123456789:;<=>?" ++
            "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_" ++
            "`abcdefghijklmnopqrstuvwxyz{|}~"

fontPath  = encodeString $ "res" </> "font" </> "SourceCodePro-Light.otf"

fontAtlas = emptyAtlas 1024 1024 (0 :: Pixel8) 5

class (Typeable i, Typeable a) => IsUpdateable i a where
    update :: i -> a -> a
    toUpdateable :: (Typeable u) => u -> Maybe a
    toUpdateable = cast


newtype Box = Box RenderEntity
    deriving (Typeable)

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


string = "Hallo Welt! :) \nline breaks"
main :: IO ()
main = 
    let scene = testScene
        conf  = defaultAppConfig{ logPriority = WARNING }
        size  = (800,600)
    in do
        state <- initialization

        font <- loadFont'
        let textE  = (textEntity font string)--{ ePosition = V3 (-5) (-3) (-10), eScale = (1/300) <$> V3 1 1 1 }
            scene' = addEntity textE scene

        (env, sc) <- execApplication "MainWireless" conf 
            $ basicWindowLoop size hints (renderEnv state, scene') loop

        finalization state
        where 
            loop win (env, scene) inputState = do
                let (scene', env') = (scene, env) `updateScene` inputState
                (_, _, l) <- io $ runRenderer (undefined {--renderScene scene'--}) env'
                
                return (env, scene')
                --unless (isEmptyRenderLog l) $ mapM_ debugM $ rlog'log l
            loadFont' = 
                let descr = FontDescriptor (12*64) (1024,1024)
                in loadFont fontPath descr
            


updateScene :: (RenderScene, RenderEnv) -> InputState -> (RenderScene, RenderEnv)
updateScene (scene, env) input =
    let ents    = entities scene
        updateF = tryWithSomeRenderable (update input :: Box -> Box)
        scene'  = scene { entities = map updateF ents
                        , sceneTime = 0.001 + sceneTime scene
                        }
        env'  = env{ envConfig = (envConfig env){confWireframe = input `isPressed` Key'W }}
    in (scene', env')        



testScene :: RenderScene
testScene = fill emptyRenderScene
    where
    fill scene = 
        let box1     = Box $ boxEntity 
                            { eScale    = (*1.5) <$> V3 1 1 1
                            , ePosition = V3 (-3) 0 (-10)
                            }
            box2     = Box $ boxEntity 
                            { eScale    = (*1.5) <$> V3 1 1 1
                            , ePosition = V3 (3) 0 (-10)
                            }
        in scene{entities = [SomeRenderable box1, SomeRenderable box2]}


boxEntity = 
    let shader    = ShaderResource "src/glsl/baseTex.vert" "src/glsl/baseTex.frag"       
        shdef     = ShaderDefinition perspectiveUniformDef
        rdef      = RenderDefinition
            { def'data     = makeMesh 4711 "cube" cubeMesh
            , def'program  = (shader, shdef)
            , def'textures = [ TextureDefinition (0, "textures") 
                              (TextureFile ("res" </> "Brown_Leather_Texture.png"))
                             ]
            }
    in mkRenderEntity rdef


textEntity font text =
    let markup            = FontMarkup 0.9 0.8
        Right fontTexture = generateFontTexture font markup Monochrome fontchars fontAtlas
        fontShader        = ShaderResource "src/glsl/baseFont.vert" "src/glsl/baseFont.frag"
        fontShaderDef     = ShaderDefinition screenSpaceDef
        
        program           = (fontShader, fontShaderDef)
        texDef            = [TextureDefinition (0, "textures") (TextureImage "some-font" (fontTexture^.textureData))]
        
        textBuff          = emptyTextBuffer fontTexture `writeText` text
        textMesh          = makeMesh 66 "fontyfont" $ textBuff^.tbufMesh
        renderDef         = RenderDefinition textMesh program texDef
    in mkRenderEntity renderDef


---------------------------------------------------------------------------------------------------

perspectiveUniformDef = do
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

screenSpaceDef = do
    ShaderEnv{..} <- shaderEnv
    let RenderScene{..} = shaderEnv'CurrentScene
        Just (RenderEntity{..}) = fromRenderable shaderEnv'CurrentRenderable
        scaleM        = kronecker . point $ eScale
        projectionM   = projectionMatrix
        viewM         = viewMatrix
        transM        = mkTransformation eOrientation ePosition
        modelM        = transM !*! scaleM 
        normalM  = fromTransformation modelM -- adjoint <$> (inv33 . fromTransformation $ modelM)
    --io $ print ePosition
    --io $ print eScale
    "projection_matrix" != projectionM
    "view_matrix"       != viewM
    "model_matrix"      != modelM
    "normal_matrix"     != normalM
    -- "normal_matrix"      != normalM

