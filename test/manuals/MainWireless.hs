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
import Yage.Font.Buffer
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

fontPath  = encodeString $ "res" </> "font" </> "SourceCodePro-Regular.otf"

fontAtlas = emptyAtlas 1024 1024 (0 :: Pixel8) 5

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

        font <- loadFont'
        let textE  = (textEntity font "Hallo Welt! gypq&%~^"){ ePosition = V3 (-5) (-5) (-10), eScale = V3 (1/200) (1/200) (1/200) }
            scene' = addEntity textE scene

        (_, st, sc) <- execApplication "MainWireless" conf 
            $ basicWindowLoop size hints (renderEnv state, renderState state, scene') loop

        finalization state
        where 
            loop win (env, st, scene) inputState = do
                let scene' = scene `updateScene` inputState
                (_, st', l) <- io $ runRenderer (renderScene scene') st env
                
                return (env, st', scene')
                --unless (isEmptyRenderLog l) $ mapM_ debugM $ rlog'log l
            loadFont' = 
                let descr = FontDescriptor (12*64) (1024,1024)
                in loadFont fontPath descr
            


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
        let box1     = Box $ boxEntity 
                            { eScale    = V3 1 1 1
                            , ePosition = V3 (-3) 0 (-10)
                            }
            box2     = Box $ boxEntity 
                            { eScale    = V3 1 1 1
                            , ePosition = V3 (3) 0 (-10)
                            }
        in scene{entities = [SomeRenderable box1, SomeRenderable box2]}


boxEntity = 
    let shader    = ShaderResource "src/glsl/baseTex.vert" "src/glsl/baseTex.frag"       
        shdef     = ShaderDefinition globalAttribDef perspectiveUniformDef

        rdef      = RenderDefinition
            { def'ident    = "cube-base"
            , def'data     = cubeMesh
            , def'program  = (shader, shdef)
            , def'textures = [ TextureDefinition (0, "textures") 
                              (TextureFile ("res" </> "Brown_Leather_Texture.png"))
                             ]
            }
    in mkRenderEntity rdef


textEntity font text =
    let Right fontTexture = generateFontTexture font Monochrome fontchars fontAtlas
        fontShader        = ShaderResource "src/glsl/baseFont.vert" "src/glsl/baseFont.frag"
        fontShaderDef     = ShaderDefinition globalAttribDef screenSpaceDef
        program           = (fontShader, fontShaderDef)
    in mkRenderEntity $ ((simpleTextBuffer fontTexture program text)^.fbufRenderDef)--{def'data = cubeMesh}


---------------------------------------------------------------------------------------------------

globalAttribDef = 
    [ "in_vert_position"  ^:= _position
    , "in_vert_normal"    ^:= _normal
    , "in_vert_color"     ^:= _color
    , "in_vert_texture"   ^:= _texture
    ]
{--
textAttribDef = 
    [ "in_vert_position"  ^:= _position
    , "in_vert_texture"   ^:= _texture
    ]
--}

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

