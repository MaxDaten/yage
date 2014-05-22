{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Yage.Resources
    ( YageResources, runYageResources
    , ResourceLoader(..), ResourceRegistry
    , MeshResource (..), MeshFileType(..), TextureResource, HasResources(..)
    , requestMeshResource, requestTextureResource
    , initialRegistry
    ) where

import           Yage.Lens
import           Yage.Prelude             hiding (Index)

import           Data.Digest.XXHash
import qualified Data.Trie                as T

import           Codec.Picture

import           Control.Monad.RWS.Strict
import qualified Data.Map.Strict          as M

import           Yage.Rendering.Mesh
import           Yage.Rendering.Resources hiding (loadedTextures)

import           Yage.Images


data ResourceRegistry geo = ResourceRegistry
    { loadedMeshes   :: M.Map XXHash (Mesh geo)
    , loadedTextures :: T.Trie Texture
    }


data MeshFileType =
      OBJFile
    | YGMFile


data MeshResource geo  = 
      MeshFile FilePath MeshFileType
    | MeshPure (Mesh geo)


data TextureResource =
      TextureFile FilePath
    | TexturePure Texture


type YageResources geo = RWST (ResourceLoader geo) () (ResourceRegistry geo) IO

type MeshLoader geo = FilePath -> IO (Mesh geo)


type ResourceLoaderAccessor geo = ResourceLoader geo -> MeshLoader geo

data ResourceLoader geo = ResourceLoader
    { objLoader :: MeshLoader geo
    , ygmLoader :: MeshLoader geo
    }


class HasResources geo resource loaded | resource -> loaded where
    requestResources :: resource -> YageResources geo loaded


{--
## Loading
--}

runYageResources :: (MonadIO m) => ResourceLoader geo -> YageResources geo a -> ResourceRegistry geo -> m (a, ResourceRegistry geo)
runYageResources loader yr st = do
    (a, res, ()) <- io $ runRWST yr loader st
    return (a, res)


requestMeshResource :: MeshResource geo -> YageResources geo (Mesh geo)
requestMeshResource (MeshFile filepath meshType) = loadMeshFile meshType filepath
requestMeshResource (MeshPure mesh) = return mesh


requestTextureResource :: TextureResource -> YageResources geo Texture
requestTextureResource (TextureFile filepath)      = loadTextureFile filepath
requestTextureResource (TexturePure alreadyLoaded) = return alreadyLoaded


loadTextureFile :: FilePath -> YageResources geo Texture
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
            Right img   -> return $ Texture (fromStrict $ fpToText f) $ Texture2D img



loadMeshFile :: MeshFileType -> FilePath -> YageResources geo (Mesh geo)
loadMeshFile = \case
    YGMFile -> loadMesh' ygmLoader
    OBJFile -> loadMesh' objLoader


loadMesh' :: ResourceLoaderAccessor geo -> FilePath -> YageResources geo (Mesh geo)
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

