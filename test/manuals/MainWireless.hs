{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import qualified Prelude
import           Yage.Prelude

import           Yage

import           Data.List
import           Data.Typeable


import           Linear
import           Yage.Font
import           Yage.Math
import           Yage.Rendering
import           Yage.Rendering.Backend.Renderer
import           Yage.Rendering.Mesh
import           Yage.Rendering.Primitives
import           Yage.Rendering.RenderScene
import           Yage.Rendering.Texture
import           Yage.Rendering.VertexSpec
import           Yage.Texture.Atlas
import           Yage.Types

import           Yage.Core.Application
import           Yage.Core.Application.Logging
import           Yage.Core.Application.Loops

hints :: [WindowHint]
hints = [ WindowHint'ContextVersionMajor  3
        , WindowHint'ContextVersionMinor  2
        , WindowHint'OpenGLProfile        OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat  True
        , WindowHint'RefreshRate          60
        --, WindowHint'Resizable            False
        --, WindowHint'Decorated            False
        ]

fontchars :: String
fontchars = " !\"#$%&'()*+,-./0123456789:;<=>?" ++
            "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_" ++
            "`abcdefghijklmnopqrstuvwxyz{|}~"
fontPath :: String
fontPath  = encodeString $ "res" </> "font" </> "SourceCodePro-Light.otf"

fontAtlas :: TextureAtlas i Pixel8
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
        in  Box ent{ _entityOrientation = signorm $ _entityOrientation * rot }


tryWithSomeRenderable :: (Typeable u, Renderable r) => (u -> r) -> SomeRenderable -> SomeRenderable
tryWithSomeRenderable f some = maybe some (toRenderable . f) (fromRenderable some)

string :: Text
string = "Hallo Welt! :) \nline breaks"
main :: IO ()
main =
    let scene  = testScene
        conf   = defaultAppConfig{ logPriority = WARNING }
        size   = (800,600)
        factor = 2
    in do
        state <- initialization $ RenderTarget (0,0) size factor 0.1 100 True
        font  <- loadFont'
        let textE  = (textEntity font string) & entityPosition .~ V3 (-5) (-3) (-10) & entityScale .~ (V3 1 1 1) / 300
            floorE = floorEntity & entityScale .~ 10 * V3 1 1 1
            scene' = scene `addEntity` floorE `addEntity` textE 

        (state', _sc) <- execApplication "MainWireless" conf
            $ basicWindowLoop size hints (state, scene') loop

        finalization state'
        where
            loop _win (yst, scene) (inputState, winEvents) = do
                let rUnit          = yst^.renderUnit
                    rSettings'     = (rUnit^.renderSettings) `updateSettings` (inputState, winEvents)
                    scene'         = scene `updateScene` inputState

                unit' <- renderScene scene' $ rUnit & renderSettings .~ rSettings'

                return (yst & renderUnit .~ unit', scene')
                --unless (isEmptyRenderLog l) $ mapM_ debugM $ rlog'log l
            loadFont' =
                let descr = FontDescriptor (12*64) (1024,1024)
                in loadFont fontPath descr

---------------------------------------------------------------------------------------------------
-- Input & Events


updateSettings :: RenderEnv -> (InputState, WindowEvents) -> RenderEnv
updateSettings env (inputSt, winEvents) =
    env & reRenderConfig.rcConfWireframe .~ inputSt `isPressed` Key'F1
        & reRenderTarget.targetSize      %?~ justResizedTo winEvents


updateScene :: RenderScene -> InputState -> RenderScene
updateScene scene inputSt =
    let ents    = scene^.sceneEntities
        updater = tryWithSomeRenderable (update inputSt :: Box -> Box)
    in scene & sceneEntities    .~ map updater ents
             & sceneTime        +~ 0.001 -- dummy'
             & sceneCamera      %~ cameraMovement inputSt
    where
        cameraMovement input cam =
            let moveV k v   = if input `isPressed` k then v else Linear.zero
                scalarD k v = if input `isPressed` k then v else 0
                speed       = 0.1 -- yes this is not frame independent
                movement    =  normalize $
                               moveV Key'W (V3 0 0 (-1))
                             + moveV Key'A (V3 (-1) 0 0)
                             + moveV Key'S (V3 0 0 1)
                             + moveV Key'D (V3 1 0 0)
                turn        =  scalarD Key'Q 1
                             + scalarD Key'E (-1)
                tilting     =  scalarD Key'R 1
                             + scalarD Key'F (-1)
                fov         =  deg2rad $
                               scalarD Key'X 1
                             + scalarD Key'Z (-1)
                camHandle   = (cam^.cameraHandle)
                              `dolly` (speed * movement)
                              `pan`   turn
                              `tilt`  tilting
            in cam & cameraHandle .~ camHandle
                   & cameraFOV    +~ fov

---------------------------------------------------------------------------------------------------
-- Entity Definitions

testScene :: RenderScene
testScene = fill $ emptyRenderScene (Camera fpsCamera (deg2rad 60.0))
    where
    fill scene =
        let box1     = Box $ boxEntity & entityScale .~ 1.5 * V3 1 1 1 & entityPosition .~ V3 (-3) 0 (-10)
            box2     = Box $ boxEntity & entityScale .~ 1.5 * V3 1 1 1 & entityPosition .~ V3 (3) 0 (-10)
        in scene & sceneEntities .~ [SomeRenderable box1, SomeRenderable box2]


boxEntity :: RenderEntity
boxEntity =
    let shader    = ShaderResource "src/glsl/baseTex.vert" "src/glsl/baseTex.frag"
        shdef     = perspectiveUniformDef
        mesh      = cubeMesh
        attribs   = [ "in_vert_position" @= mesh^.mDataVertices^..traverse.vPosition
                    , "in_vert_normal"   @= mesh^.mDataVertices^..traverse.vNormal
                    , "in_vert_color"    @= mesh^.mDataVertices^..traverse.vColor
                    , "in_vert_texture"  @= mesh^.mDataVertices^..traverse.vTexture
                    ]
        rdef      = RenderDefinition
            { _rdefData     = makeMesh 4711 "cube" mesh attribs
            , _rdefProgram  = (shader, shdef)
            , _rdefTextures = [ TextureDefinition (0, "textures")
                               (TextureFile ("res" </> "Brown_Leather_Texture.png"))
                              ]
            , _rdefMode     = Triangles
            }
    in mkRenderEntity rdef


floorEntity :: RenderEntity
floorEntity =
    let shader      = ShaderResource "src/glsl/base.vert" "src/glsl/base.frag"
        shdef       = perspectiveUniformDef
        mesh        = gridMesh (10, 10)
        attribs     = [ "in_vert_position" @= mesh^.mDataVertices^..traverse.vPosition
                      , "in_vert_normal"   @= mesh^.mDataVertices^..traverse.vNormal
                      , "in_vert_color"    @= mesh^.mDataVertices^..traverse.vColor
                      , "in_vert_texture"  @= mesh^.mDataVertices^..traverse.vTexture
                      ]
        rdef        = RenderDefinition
                        { _rdefData     = makeMesh 0815 "floor" mesh attribs
                        , _rdefProgram  = (shader, shdef)
                        , _rdefTextures = []
                        , _rdefMode     = Triangles
                        }
    in mkRenderEntity rdef


textEntity :: Font -> Text -> RenderEntity
textEntity font text =
    let markup            = FontMarkup 0.9 0.8
        Right fontTexture = generateFontTexture font markup Monochrome fontchars fontAtlas

        fontShader        = ShaderResource "src/glsl/baseFont.vert" "src/glsl/baseFont.frag"
        fontShaderDef     = screenSpaceDef

        program           = (fontShader, fontShaderDef)
        texDef            = [TextureDefinition (0, "textures") (TextureImage "some-font" (fontTexture^.textureData))]

        textBuff          = emptyTextBuffer fontTexture `writeText` text
        mesh              = textBuff^.tbufMesh
        textMesh          = makeMesh 66 "fontyfont" mesh attribs
        attribs           = [ "in_vert_position" @= mesh^.mDataVertices^..traverse.vPosition
                            , "in_vert_color"    @= mesh^.mDataVertices^..traverse.vColor
                            , "in_vert_texture"  @= mesh^.mDataVertices^..traverse.vTexture
                            ]
        renderDef         = RenderDefinition textMesh program texDef Triangles
    in mkRenderEntity renderDef


---------------------------------------------------------------------------------------------------
-- Shader Definitions
perspectiveUniformDef :: ShaderDefinition ()
perspectiveUniformDef = do
    vdef   <- view seViewDef
    "mvp_matrix"      != vdef^.vdMVPMatrix
    "normal_matrix"   != vdef^.vdNormalMatrix


screenSpaceDef :: ShaderDefinition ()
screenSpaceDef = do
    vdef <- view seViewDef
    view <- view seView
    "projection_matrix" != view^.rvProjectionMatrix
    "view_matrix"       != view^.rvViewMatrix
    "model_matrix"      != vdef^.vdModelMatrix

