{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE LambdaCase             #-}

module Yage.Resources
  ( meshRes
  , imageRes
  , textureRes
  , materialRes
  , imageMipsRes
  , seperateCubeMipsRes
  , cubeCrossMipsRes
  , fontRes
  -- , loadOBJ
  , loadYGM
  , mkSelection
  -- * Reexprts
  , module YRes
  , module Slot
  , module Yage.Rendering.Mesh
  , module Yage.Font
  , module MipmapChain
  , module Cubemap
  ) where

import           Yage.Lens
import           Yage.Prelude                     hiding (Index)

import qualified Data.Map.Strict                  as M
import qualified Data.Set                         as S

import           Quine.Cubemap                    as Cubemap
import           Quine.MipmapChain                as MipmapChain
import           Yage.Font                        (FontTexture)
import           Yage.Geometry
import           Yage.Geometry.D2.Rectangle
import           Yage.Image
import           Yage.Material                    hiding (over)
import           Yage.Rendering.Resources.GL.Texture

import           Yage.Rendering.Mesh
import           Yage.Resource.MSlot              as Slot
import           Yage.Resource.Slot               as Slot
import           Yage.Resource.YageResource       as YRes
import           Yage.Texture.CubeImageLayout     as Cubemap
import qualified Yage.Formats.Font                as Font
import qualified Yage.Formats.Obj                 as OBJ
import qualified Yage.Formats.Ygm                 as YGM

data ResourceLoadingException =
    ImageResourceException String
  | MipMapMissingBaseException String
  deriving ( Show, Typeable )
instance Exception ResourceLoadingException

{--
data Selection =
      SelectAll
    | IncludeSelection [Text]
    | ExcludeSelection [Text]
--}
type SubMeshSelection = S.Set Text
type MeshFilePath = (FilePath, SubMeshSelection)

-- TODO : gl VBO
meshRes :: Storable v => IO (Mesh v) -> YageResource (Mesh v)
meshRes loadMesh = mkAcquire loadMesh (const $ return ())


-- | Load a image into a 'DynamicImage'
imageRes :: FilePath -> YageResource DynamicImage
imageRes filePath = mkAcquire loadImage (const $ return ())
  where
  loadImage = join $ either (throwIO.ImageResourceException) return <$> readImage (fpToString filePath)

-- | Allocate a 'Material' with a 'Image2D' instance as texture data source into a 'Material' with a render hardware texture
materialRes :: (GetRectangle i Int, Image2D i, BaseTexture i d) => Material col i -> YageResource (Material col (Texture d px))
materialRes imgMat = (set materialTexture) <$> (createTextureFromImage (imgMat^.materialTexture)) <*> pure imgMat

-- | Allocate a 'Image2D' instance into a render hardware 'Texture'
textureRes :: (GetRectangle i Int, Image2D i, BaseTexture i d) => i -> YageResource (Texture d px)
textureRes = createTextureFromImage

-- | loads a 'MipmapChain' from seperate images-files. The 'FilePath' is globbed
-- (@see 'System.FilePath.Glob') and sorted.
imageMipsRes:: FilePath -> YageResource (MipmapChain DynamicImage)
imageMipsRes fpToGlob = do
  globbed <- sort <$> globFp fpToGlob
  case mipMapChain globbed of
    Just mipmaps -> traverse imageRes mipmaps
    Nothing -> throwIO $ MipMapMissingBaseException $ "at least a base image required but globbed nothing: " ++ fpToString fpToGlob


-- | loads a 'Cube' with a 'MipmapChain's on each side. We use 'MipmapChain (Cubemap FilePath)'
-- as an automatic proove that each 'MipmapChain' on each 'Cube' face has the same length.
-- Each Cube face has a destinct file to load.
seperateCubeMipsRes :: MipmapChain (Cubemap FilePath) -> YageResource (MipmapChain (Cubemap DynamicImage))
seperateCubeMipsRes = (traverse . traverse) imageRes


cubeCrossMipsRes :: CubeImageLayout -> FilePath -> YageResource (MipmapChain (Cubemap DynamicImage))
cubeCrossMipsRes orient = (fmap.fmap) (fDynamicMap (seperateCubeMapImage orient)) . imageMipsRes

fontRes :: FilePath -> YageResource FontTexture
fontRes filePath = mkAcquire (Font.readFontTexture filePath) (const $ return ())


loadOBJ :: Storable v => (YGM.YGMVertex -> v) -> MeshFilePath -> IO (Mesh v)
loadOBJ fromInternal (filepath,subSelection) = do
  OBJ.GeometryGroup geoGroup <- OBJ.geometryFromOBJ <$> OBJ.parseOBJFile filepath
  createMesh $ M.mapKeys decodeUtf8 geoGroup
  where
  createMesh geoGroup
    | not $ isValidSelection subSelection geoGroup = error $ printf "invalid group selection: %s" (show $ subSelection S.\\ M.keysSet geoGroup)
    | otherwise = do
        let geos            = M.toList $ M.filterWithKey (isSelected subSelection) geoGroup
            tbnGeos         = over (traverse._2) (uncurry calcTangentSpaces) geos
            packed          = zipWith packer geos tbnGeos
            mesh            = emptyMesh & meshId .~ (encodeUtf8 $ fpToText filepath)
        return $ foldl' appendGeometry mesh packed

  converter p t n = fromInternal $ YGM.ygmFormat p t n
  packer (ident, (pos, tex)) (_,tbn) = (encodeUtf8 ident, undefined {--packGeos converter pos tex tbn--})


loadYGM :: Storable v => (YGM.YGMVertex -> v) -> MeshFilePath -> IO (Mesh v)
loadYGM fromInternal (filepath,subSelection) = createMesh <$> YGM.ygmFromFile filepath where
  createMesh YGM.YGM{..}
    | not $ isValidSelection subSelection ygmModels = error $ printf "invalid group selection: %s" (show $ subSelection S.\\ M.keysSet ygmModels)
    | otherwise =
      let mesh   = emptyMesh & meshId .~ encodeUtf8 ygmName
      in {-# SCC "loadYGM.fold" #-} M.foldlWithKey acc mesh ygmModels
  acc m k geo
    | (isSelected subSelection k geo) = m `appendGeometry` (encodeUtf8 k, fmap fromInternal geo)
    | otherwise = m


mkSelection :: [ Text ] -> SubMeshSelection
mkSelection = S.fromList
{-# INLINE mkSelection #-}


isSelected :: SubMeshSelection -> Text -> a -> Bool
isSelected selection key _ | S.null selection = True
                           | otherwise = key `S.member` selection
{-# INLINE isSelected #-}


isValidSelection :: SubMeshSelection -> Map Text a -> Bool
isValidSelection selection theMap = S.null $ S.difference selection (M.keysSet theMap)
{-# INLINE isValidSelection #-}


fDynamicMap :: forall f. Functor f => (forall pixel. Pixel pixel => Image pixel -> f (Image pixel)) -> DynamicImage -> f DynamicImage
fDynamicMap f = aux
  where
    aux (ImageY8    i)  = fmap ImageY8 (f i)
    aux (ImageY16   i)  = fmap ImageY16 (f i)
    aux (ImageYF    i)  = fmap ImageYF (f i)
    aux (ImageYA8   i)  = fmap ImageYA8 (f i)
    aux (ImageYA16  i)  = fmap ImageYA16 (f i)
    aux (ImageRGB8  i)  = fmap ImageRGB8 (f i)
    aux (ImageRGB16 i)  = fmap ImageRGB16 (f i)
    aux (ImageRGBF  i)  = fmap ImageRGBF (f i)
    aux (ImageRGBA8 i)  = fmap ImageRGBA8 (f i)
    aux (ImageRGBA16 i) = fmap ImageRGBA16 (f i)
    aux (ImageYCbCr8 i) = fmap ImageYCbCr8 (f i)
    aux (ImageCMYK8 i)  = fmap ImageCMYK8 (f i)
    aux (ImageCMYK16 i) = fmap ImageCMYK16 (f i)
