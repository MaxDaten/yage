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

import Control.Monad.Reader            (asks)

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
    deriving (Typeable, Renderable)


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
        font  <- loadFont'
        let textE  = (textEntity font string)--{ ePosition = V3 (-5) (-3) (-10), eScale = (1/300) <$> V3 1 1 1 }
            scene' = addEntity textE scene

        (state', sc) <- execApplication "MainWireless" conf 
            $ basicWindowLoop size hints (state, scene') loop

        finalization state'
        where 
            loop win (state@YageState{..}, scene) inputState = do
                -- this is a mess
                let env            = renderUnit^.renderSettings
                    (scene', env') = (scene, env) `updateScene` inputState
                    unit           = renderSettings .~ env' $ renderUnit
                unit' <- renderScene scene' unit
                
                return (state{ renderUnit = unit' }, scene')
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
        mesh      = cubeMesh
        attribs   = [ "in_vert_position" @= (vertices mesh)^..traverse.vPosition
                    , "in_vert_normal"   @= (vertices mesh)^..traverse.vNormal
                    , "in_vert_color"    @= (vertices mesh)^..traverse.vColor
                    , "in_vert_texture"  @= (vertices mesh)^..traverse.vTexture
                    ]
        rdef      = RenderDefinition
            { def'data     = makeMesh 4711 "cube" mesh attribs
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
        mesh              = textBuff^.tbufMesh
        textMesh          = makeMesh 66 "fontyfont" mesh attribs
        attribs           = [ "in_vert_position" @= (vertices mesh)^..traverse.vPosition
                            , "in_vert_color"    @= (vertices mesh)^..traverse.vColor
                            , "in_vert_texture"  @= (vertices mesh)^..traverse.vTexture
                            ]
        renderDef         = RenderDefinition textMesh program texDef
    in mkRenderEntity renderDef


---------------------------------------------------------------------------------------------------

perspectiveUniformDef :: UniShader ()
perspectiveUniformDef = do
    ViewDefinition{..} <- asks shaderEnv'CurrentRenderable
    RenderView{..}     <- asks shaderEnv'CurrentScene
    "projection_matrix" != _rvProjectionMatrix
    "view_matrix"       != _rvViewMatrix
    "model_matrix"      != _vdModelMatrix
    "normal_matrix"     != _vdNormalMatrix

screenSpaceDef :: UniShader ()
screenSpaceDef = do
    ViewDefinition{..} <- asks shaderEnv'CurrentRenderable
    RenderView{..}     <- asks shaderEnv'CurrentScene
    "projection_matrix" != _rvProjectionMatrix
    "view_matrix"       != _rvViewMatrix
    "model_matrix"      != _vdModelMatrix
    -- "normal_matrix"     != _vdNormalMatrix

