{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Yage.Resources
    ( YageResources, runYageResources
    , ResourceLoader(..), ResourceRegistry
    , MeshResource (..), MeshFileType(..), TextureResource(..), HasResources(..)
    , requestMeshResource, requestTextureResource
    , initialRegistry
    ) where

import           Yage.Lens
import           Yage.Prelude             hiding (Index)

import           Data.Digest.XXHash
import qualified Data.Trie                as T

import           Codec.Picture

import           Control.Monad.RWS.Strict hiding (mapM)
import qualified Data.Map.Strict          as M

import           Yage.Rendering.Mesh
import           Yage.Rendering.Resources hiding (loadedTextures)

import           Yage.Images


data ResourceRegistry vert = ResourceRegistry
    { loadedMeshes   :: M.Map XXHash (Mesh vert)
    , loadedTextures :: T.Trie Texture
    }


data MeshFileType =
      OBJFile
    | YGMFile


data MeshResource vert = 
      MeshFile FilePath MeshFileType
    | MeshPure (Mesh vert)


data TextureResource =
      TextureFile FilePath
    | TexturePure Texture


type YageResources vert = RWST (ResourceLoader vert) () (ResourceRegistry vert) IO

type MeshLoader vert = FilePath -> IO (Mesh vert)


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
requestMeshResource (MeshFile filepath meshType) = loadMeshFile meshType filepath
requestMeshResource (MeshPure mesh) = return mesh


requestTextureResource :: TextureResource -> YageResources vert Texture
requestTextureResource (TextureFile filepath)      = loadTextureFile filepath
requestTextureResource (TexturePure alreadyLoaded) = return alreadyLoaded


loadTextureFile :: FilePath -> YageResources vert Texture
loadTextureFile f = do
    let filepath = encodeUtf8 . fpToText $ f

    registry <- get
    res <- maybe
            (load $ fpToString f)
            return
            (registry^.textures.at filepath)

    put $ registry & textures.at filepath ?~ res
    return res
    where
    load path = io $ do
        eImg <- (fromDynamic =<<) <$> readImage path
        case eImg of
            Left err    -> error err
            Right img   -> return $ mkTexture (fromStrict $ fpToText f) $ Texture2D img


loadMeshFile :: MeshFileType -> FilePath -> YageResources vert (Mesh vert)
loadMeshFile = \case
    YGMFile -> loadMesh' ygmLoader
    OBJFile -> loadMesh' objLoader


loadMesh' :: ResourceLoaderAccessor vert -> FilePath -> YageResources vert (Mesh vert)
loadMesh' loader filepath = do
    xhash <- xxHash <$> readFile filepath
    registry <- get
    res <- maybe
            load
            return
            (registry^.meshes.at xhash)

    put $ registry & meshes.at xhash ?~ res
    return res

    where
    load = do
        l <- (asks loader)
        io $ l filepath

{--
## Utility
--}

initialRegistry :: ResourceRegistry geo
initialRegistry = ResourceRegistry M.empty T.empty

meshes :: Lens' (ResourceRegistry geo) (M.Map XXHash (Mesh geo))
meshes = lens loadedMeshes (\r m -> r{ loadedMeshes = m })

textures :: Lens' (ResourceRegistry geo) (T.Trie Texture)
textures = lens loadedTextures (\r t -> r{ loadedTextures = t })


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

{--
instance (Traversable f, HasResources vert a a') => HasResources vert (f a) (f a') where
    requestResources = mapM requestResources
--}
