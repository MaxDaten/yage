{-# OPTIONS_GHC -funbox-strict-fields            #-}
{-# LANGUAGE TemplateHaskell #-}
module Yage.UI.GUI
    ( module Yage.UI.GUI
    , module Yage.Font
    ) where

import           Yage.Prelude
import           Yage.Lens
import           Yage.Math               hiding ( lerp )

import           Data.Data
import           Yage.Camera
import           Yage.Font
import           Yage.Geometry
import           Yage.Image
import           Yage.Rendering.Mesh
import           Yage.Transformation
import           Quine.GL.Types
import           Foreign.Storable
import           Foreign.Storable.Generic
import qualified Data.Map.Lazy           as M

data GUIVertex = GUIVertex
  { guiPosition :: !Vec2
  , guiTexture  :: !Vec2
  , guiColor    :: !Vec4
  } deriving (Eq,Ord,Show,Data,Typeable,Generic)

makeLenses ''GUIVertex

instance Storable GUIVertex where
  alignment = alignmentDefault
  sizeOf = sizeOfDefault
  peek = peekDefault
  poke = pokeDefault

data GUIElementType = TXT | SDF | IMG
  deriving (Eq,Ord,Show,Enum,Data,Typeable,Generic)

-- REMOVE ME
type Texture = DynamicImage

data GUIElement =
    GUIFont   TextBuffer ( Transformation Double )
  | GUISDF   ( Mesh GUIVertex, Texture ) ( Transformation Double )
  | GUIImage ( Mesh GUIVertex, Texture ) ( Transformation Double )
  deriving (Generic)

instance HasTransformation GUIElement Double where
  transformation = lens getter setter where
    getter (GUIFont _  t) = t
    getter (GUISDF _   t) = t
    getter (GUIImage _ t) = t
    setter (GUIFont x  _) t = (GUIFont x t)
    setter (GUISDF x   _) t = (GUISDF x t)
    setter (GUIImage x _) t = (GUIImage x t)

instance HasPosition GUIElement (V3 Double) where
  position = transformation.position

instance HasOrientation GUIElement (Quaternion Double) where
  orientation = transformation.orientation

instance HasScale GUIElement (V3 Double) where
  scale = transformation.scale

data GUI = GUI
  { _guiCamera   :: Camera
  , _guiElements :: Map ByteString GUIElement
  } deriving (Generic)

makeLenses ''GUI

instance HasCamera GUI where
    camera = guiCamera

emptyGUI :: GUI
emptyGUI = GUI (mkCameraGui 1.0001 (-1.0001)) M.empty


-- | creates a camera suitable for 2D gui rendering.
--
-- for gui rendering it's a common practice to stack the elements along the z-axis
-- like layering in photoshop.
-- The layer range is defined by the near and far plane.
mkCameraGui :: Double -> Double -> Camera
mkCameraGui= idCamera (deg2rad 90)


guiImageSDF :: Texture -> V4 Double -> V2 Double -> V2 Double -> GUIElement
guiImageSDF img color pos size =
    let trans = idTransformation & scale._xy    .~ size
                                 & position._xy .~ pos
    in GUISDF  ( unitColorQuad color, img ) trans


guiImage :: Texture -> V4 Double -> V2 Double -> V2 Double -> GUIElement
guiImage img color pos size =
    let trans = idTransformation & scale._xy    .~ size
                                 & position._xy .~ pos
    in GUIImage  ( unitColorQuad color, img ) trans


guiImageId :: Texture -> V4 Double -> V2 Double -> GUIElement
guiImageId img color pos =
    let size  = fromIntegral <$> (img^.asRectangle.xy2)
        trans = idTransformation & scale._xy    .~ size
                                 & position._xy .~ pos
    in GUIImage  ( unitColorQuad color, img ) trans

elementLayer :: Lens' GUIElement Double
elementLayer = transformation.position._z

unitColorQuad :: V4 Double -> Mesh GUIVertex
unitColorQuad color = mkFromVerticesF "GUI.UNIT.QUAD" . vertices . triangles $ targetFace
    where
    targetFace  :: Face GUIVertex
    targetFace  = Face
        ( GUIVertex (V2 0 1) (V2 0 1) (realToFrac <$> color) )
        ( GUIVertex (V2 0 0) (V2 0 0) (realToFrac <$> color) )
        ( GUIVertex (V2 1 0) (V2 1 0) (realToFrac <$> color) )
        ( GUIVertex (V2 1 1) (V2 1 1) (realToFrac <$> color) )
{-# INLINE unitColorQuad #-}


instance LinearInterpolatable GUI where
    lerp alpha u v =
        u & guiCamera   .~ lerp alpha (u^.guiCamera) (v^.guiCamera)
          & guiElements .~ M.intersectionWith (lerp alpha) (u^.guiElements) (v^.guiElements)

instance LinearInterpolatable GUIElement where
    lerp alpha u v = u & transformation .~ lerp alpha (u^.transformation) (v^.transformation)
