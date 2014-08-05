{-# LANGUAGE TemplateHaskell #-}
module Yage.UI.GUI
    ( module Yage.UI.GUI
    , module Yage.Font
    ) where

import           Yage.Prelude
import           Yage.Lens
import           Yage.Math               hiding ( lerp )

import           Yage.Font
import           Yage.Camera
import           Yage.Geometry
import           Yage.Rendering
import           Yage.Transformation
import qualified Data.Map.Lazy           as M
import           Yage.Resources
import           Yage.Rendering.Textures

type GUIVertex = Vertex (Y'P2TX2C4 GLfloat)

data GUIElementType = TXT | SDF | IMG
    deriving ( Show, Enum )

data GUIElement =
      GUIFont TextBuffer ( Transformation Float )
    | GUISDF ( Mesh GUIVertex, Texture ) (Transformation Float)
    | GUIImage ( Mesh GUIVertex, Texture ) (Transformation Float)
    deriving ( Show )

data GUI = GUI
    { _guiCamera   :: Camera
    , _guiElements :: Map ByteString GUIElement
    } deriving ( Show )

makeLenses ''GUI

emptyGUI :: GUI
emptyGUI = GUI (mkCameraGui (-1, 1)) M.empty


-- | creates a camera suitable for 2D gui rendering.
--
-- for gui rendering it's a common practice to stack the elements along the z-axis
-- like layering in photoshop.
-- The layer range is defined by the near and far plane.
-- The camera is looking along the positive z-axis. To make layering more intuitive
-- the camera is moved to z=far.
-- ordering of viewable layers is in the ascending range [0..100] (floating)
mkCameraGui :: (Double, Double) -> Camera
mkCameraGui nearFar@(near, _) =
    -- fov is not needed for 2d, viewfield in 2d just defined with ortho-matrix
    mkCameraFps 90 nearFar idTransformation
        & cameraLocation._z .~ realToFrac near


guiImageSDF :: Texture -> V4 Float -> V2 Float -> V2 Float -> GUIElement
guiImageSDF img color pos size =
    let trans = idTransformation & transScale._xy    .~ size
                                 & transPosition._xy .~ pos
    in GUISDF  ( unitColorQuad color, img ) trans


guiImage :: Texture -> V4 Float -> V2 Float -> V2 Float -> GUIElement
guiImage img color pos size =
    let trans = idTransformation & transScale._xy    .~ size
                                 & transPosition._xy .~ pos
    in GUIImage  ( unitColorQuad color, img ) trans


guiImageId :: Texture -> V4 Float -> V2 Float -> GUIElement
guiImageId img color pos =
    let size  = fromIntegral <$> img^.textureSpec.texSpecDimension
        trans = idTransformation & transScale._xy    .~ size
                                 & transPosition._xy .~ pos
    in GUIImage  ( unitColorQuad color, img ) trans

elementTransformation :: Lens' GUIElement (Transformation Float)
elementTransformation = lens getter setter where
    getter (GUIFont _ trans)    = trans
    getter (GUISDF _ trans)     = trans
    getter (GUIImage _ trans)   = trans

    setter (GUIFont  a _) trans  = GUIFont a trans
    setter (GUISDF   a _) trans  = GUISDF a trans
    setter (GUIImage a _) trans  = GUIImage a trans


elementPosition :: Lens' GUIElement (V3 Float)
elementPosition = elementTransformation.transPosition

elementScale :: Lens' GUIElement (V2 Float)
elementScale = elementTransformation.transScale._xy

elementOrientation :: Lens' GUIElement (Quaternion Float)
elementOrientation = elementTransformation.transOrientation

unitColorQuad :: V4 Float -> Mesh GUIVertex
unitColorQuad color = mkFromVerticesF "GUI.UNIT.QUAD" . vertices . triangles $ targetFace
    where
    targetFace  :: Face GUIVertex
    targetFace  = Face
        ( position2 =: V2 0 1 <+> texture2 =: V2 0 1 <+> color4 =: (realToFrac <$> color) )
        ( position2 =: V2 0 0 <+> texture2 =: V2 0 0 <+> color4 =: (realToFrac <$> color) )
        ( position2 =: V2 1 0 <+> texture2 =: V2 1 0 <+> color4 =: (realToFrac <$> color) )
        ( position2 =: V2 1 1 <+> texture2 =: V2 1 1 <+> color4 =: (realToFrac <$> color) )


instance LinearInterpolatable GUI where
    lerp alpha u v =
        u & guiCamera   .~ lerp alpha (u^.guiCamera) (v^.guiCamera)
          & guiElements .~ M.intersectionWith (lerp alpha) (u^.guiElements) (v^.guiElements)

instance LinearInterpolatable GUIElement where
    lerp alpha u v = u & elementTransformation .~ lerp alpha (u^.elementTransformation) (v^.elementTransformation)

instance HasResources vert GUI GUI where
    requestResources = return
