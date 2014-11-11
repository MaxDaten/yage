{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE NamedFieldPuns         #-}

module Yage.Resources
    ( module Acquire
    , module Yage.Resources
    , module Yage.Rendering.Mesh
    , module Yage.Rendering.Resources
    , module Yage.Font
    , Cube(..), CubeImageLayout(..)
    ) where

import           Yage.Lens
import           Yage.Prelude                     hiding (Index)

import           Data.Acquire                     as Acquire
import           Control.Monad.Trans.Resource     as Acquire

import qualified Data.Map.Strict                  as M
import qualified Data.Set                         as S

import           Yage.Geometry
import           Yage.Rendering.Mesh
import           Yage.Rendering.Resources

import qualified Yage.Formats.Obj                 as OBJ
import qualified Yage.Formats.Ygm                 as YGM
import qualified Yage.Formats.Font                as Font
import           Yage.Font                        ( FontTexture )
import           Yage.Texture
import           Yage.Texture.CubeImageLayout


import           Yage.Images



type YageResource = Acquire
data ResourceLoadingException = MipMapMissingBaseException String deriving ( Show, Typeable )
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
meshRes :: Storable (Vertex v) => IO (Mesh (Vertex v)) -> YageResource (Mesh (Vertex v))
meshRes loadMesh = mkAcquire loadMesh (const $ return ())


-- TODO : GL Texture resource
imageRes :: FilePath -> YageResource TextureImage
imageRes filePath = mkAcquire (loadImage filePath) (const $ return ())


-- | loads a 'MipMapChain' from seperate images-files. The 'FilePath' is globbed
-- (@see 'System.FilePath.Glob') and sorted.
imageMipsRes:: FilePath -> YageResource (MipMapChain TextureImage)
imageMipsRes fpToGlob = do
    globbed <- sort <$> globFp fpToGlob
    case mipMapChain globbed of
        Just mipmaps -> traverse imageRes mipmaps
        Nothing -> throwIO $ MipMapMissingBaseException $
                        "at least a base image required but globbed nothing: " ++ fpToString fpToGlob


-- | loads a 'Cube' with a 'MipMapChain's on each side. We use 'MipMapChain (Cube FilePath)'
-- as an automatic proove that each 'MipMapChain' on each 'Cube' face has the same length.
-- Each Cube face has a destinct file to load.
seperateCubeMipsRes :: MipMapChain (Cube FilePath) -> YageResource (MipMapChain TextureCube)
seperateCubeMipsRes = (traverse . traverse) imageRes


cubeCrossMipsRes :: CubeImageLayout -> FilePath -> YageResource (MipMapChain TextureCube)
cubeCrossMipsRes orient = (fmap.fmap) (seperateCubeMapImage orient) . imageMipsRes


fontRes :: FilePath -> YageResource FontTexture
fontRes filePath = mkAcquire (Font.readFontTexture filePath) (const $ return ())


loadOBJ :: ( Storable (Vertex v) ) => (Vertex YGM.YGMFormat -> Vertex v) -> MeshFilePath -> IO (Mesh (Vertex v))
loadOBJ fromInternal (filepath,subSelection) = do
    OBJ.GeometryGroup geoGroup <- OBJ.geometryFromOBJ <$> OBJ.parseOBJFile filepath
    createMesh $ M.mapKeys decodeUtf8 geoGroup

    where

    createMesh geoGroup
        | not $ isValidSelection subSelection geoGroup = error $ unpack $ format "invalid group selection: {}" (Only $ Shown $ subSelection S.\\ M.keysSet geoGroup)
        | otherwise = do
            let geos            = M.toList $ M.filterWithKey (isSelected subSelection) geoGroup
                tbnGeos         = over (traverse._2) (uncurry calcTangentSpaces) geos
                packed          = zipWith packer geos tbnGeos
                mesh            = emptyMesh & meshId .~ (encodeUtf8 $ fpToText filepath)
            return $ foldl' appendGeometry mesh packed

    converter p t n = fromInternal $ YGM.internalFormat p t n

    packer (ident, (pos, tex)) (_,tbn) = (encodeUtf8 ident, packGeos converter pos tex tbn)


loadYGM :: ( Storable (Vertex v) ) => (Vertex YGM.YGMFormat -> Vertex v) -> MeshFilePath -> IO (Mesh (Vertex v))
loadYGM fromInternal (filepath,subSelection) = createMesh <$> YGM.ygmFromFile filepath where
    createMesh YGM.YGM{..}
        | not $ isValidSelection subSelection ygmModels = error $ unpack $ format "invalid group selection: {}" (Only $ Shown $ subSelection S.\\ M.keysSet ygmModels)
        | otherwise =
            let geoMap = convertVertices <$> M.filterWithKey (isSelected subSelection) ygmModels
                mesh   = emptyMesh & meshId .~ encodeUtf8 ygmName
            in  M.foldlWithKey (\m k geo -> m `appendGeometry` (encodeUtf8 k, geo)) mesh geoMap

    convertVertices = geoVertices %~ map fromInternal


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

