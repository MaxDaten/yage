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
    , Cube(..)
    ) where

import           Yage.Lens
import           Yage.Prelude                     hiding (Index)

import           Data.Acquire                     as Acquire
import           Codec.Picture

import qualified Data.Map.Strict                  as M
import qualified Data.Set                         as S

import           Yage.Geometry
import           Yage.Rendering.Mesh
import           Yage.Rendering.Resources

import qualified Yage.Formats.Obj                 as OBJ
import qualified Yage.Formats.Ygm                 as YGM


import           Yage.Images



type YageResource = Acquire
-- type MeshResource v = YageResource (Mesh v)
-- type TextureResource = YageResource Texture

{--
data Selection =
      SelectAll
    | IncludeSelection [Text]
    | ExcludeSelection [Text]
--}
type SubMeshSelection = S.Set Text
type MeshFilePath = (FilePath, SubMeshSelection)

-- TODO : gl VBO
meshResource :: Storable (Vertex v) => IO (Mesh (Vertex v)) -> YageResource (Mesh (Vertex v))
meshResource loadMesh = mkAcquire loadMesh (const $ return ())



textureResource :: FilePath -> YageResource Texture
textureResource filePath = mkAcquire loadTexture (const $ return ()) where
    loadTexture = do
        eImg <- (fromDynamic =<<) <$> readImage (fpToString filePath)
        case eImg of
            Left err    -> error err
            Right img   -> return $ mkTexture (encodeUtf8 $ fpToText filePath) $ Texture2D img



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


-- | extracts the `TextureImages` from the `Cube` `Texture` fields and creates
-- a new Texture with Cube `TextureData`
cubeTexture :: Cube Texture -> Texture
cubeTexture cubeTexs@Cube{cubeFaceRight} =
    let cubeImgs = cubeTexs & mapped %~ ( \tex -> getTextureImg $ tex^.textureData )
    in mkTexture (cubeFaceRight^.textureId ++ "-CubeMap") $ TextureCube $ cubeImgs
    where
    getTextureImg (Texture2D img) = img
    getTextureImg _ = error "requestResources: invalid TextureData"

{--
data MeshResource vert =
      MeshFile MeshFilePath MeshFileType
    | MeshPure (Mesh vert)


meshFile :: FilePath -> SubMeshSelection -> Either String (YageResource (Mesh vert))
meshFile filepath selection =

data ResourceRegistry vert = ResourceRegistry
    { loadedMeshes   :: M.Map XXHash (Mesh vert)
    , loadedTextures :: T.Trie Texture
    }


data MeshFileType =
      OBJFile
    | YGMFile



data TextureResource =
      TextureFile FilePath
    | TexturePure Texture


type YageResources vert = RWST (ResourceLoader vert) () (ResourceRegistry vert) IO

type MeshLoader vert = MeshFilePath -> IO (Mesh vert)


type ResourceLoaderAccessor vert = ResourceLoader vert -> MeshLoader vert

data ResourceLoader vert = ResourceLoader
    { objLoader :: MeshLoader vert
    , ygmLoader :: MeshLoader vert
    }


class HasResources vert resource loaded | resource -> loaded where
    requestResources :: resource -> YageResources vert loaded


{--
## Loading
--}

runYageResources :: (MonadIO m) => ResourceLoader vert -> YageResources vert a -> ResourceRegistry vert -> m (a, ResourceRegistry vert)
runYageResources loader yr st = do
    (a, res, ()) <- io $ runRWST yr loader st
    return (a, res)


requestMeshResource :: MeshResource vert -> YageResources vert (Mesh vert)
requestMeshResource (MeshFile path meshType) = loadMeshFile meshType path
requestMeshResource (MeshPure mesh) = return mesh


requestTextureResource :: TextureResource -> YageResources vert Texture
requestTextureResource (TextureFile filepath)      = loadTextureFile filepath
requestTextureResource (TexturePure alreadyLoaded) = return alreadyLoaded


loadTextureFile :: FilePath -> YageResources vert Texture
loadTextureFile f = do
    let filepath = encodeUtf8 . fpToText $ f

    registry <- get
    res <- maybe
            (load)
            return
            (registry^.textures.at filepath)

    put $ registry & textures.at filepath ?~ res
    return res
    where
    load = io $ do
        eImg <- (fromDynamic =<<) <$> readImage (fpToString f)
        case eImg of
            Left err    -> error err
            Right img   -> return $ mkTexture (encodeUtf8 $ fpToText f) $ Texture2D img


loadMeshFile :: MeshFileType -> MeshFilePath -> YageResources vert (Mesh vert)
loadMeshFile = \case
    YGMFile -> loadMesh' ygmLoader
    OBJFile -> loadMesh' objLoader


loadMesh' :: ResourceLoaderAccessor vert -> MeshFilePath -> YageResources vert (Mesh vert)
loadMesh' loader path = do
    let xhash = hashPath path
    registry <- get
    res <- maybe
            (printIOTime load)
            return
            (registry^.meshes.at xhash)

    put $ registry & meshes.at xhash ?~ res
    return res

    where
    load = do
        l <- asks loader
        io $ l path

{--
## Utility
--}

hashPath :: MeshFilePath -> XXHash
hashPath (filepath, subs) =
    let pathBS = encodeUtf8 . fpToText $ filepath
    in xxHash' (concat $ pathBS:(map encodeUtf8 $ S.toList subs))

initialRegistry :: ResourceRegistry geo
initialRegistry = ResourceRegistry M.empty T.empty

meshes :: Lens' (ResourceRegistry geo) (M.Map XXHash (Mesh geo))
meshes = lens loadedMeshes (\r m -> r{ loadedMeshes = m })

textures :: Lens' (ResourceRegistry geo) (T.Trie Texture)
textures = lens loadedTextures (\r t -> r{ loadedTextures = t })


mkSelection :: [ Text ] -> SubMeshSelection
mkSelection = S.fromList

instance HasResources vert (MeshResource vert) (Mesh vert) where
    requestResources = requestMeshResource

instance HasResources vert TextureResource Texture where
    requestResources = requestTextureResource

instance HasResources vert (Mesh v) (Mesh v) where
    requestResources = return

instance HasResources vert Texture Texture where
    requestResources = return

instance HasResources vert () () where
    requestResources _ = return ()

--}
