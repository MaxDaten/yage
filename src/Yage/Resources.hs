{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Yage.Resources
    ( YageResources, runYageResources
    , ResourceLoader(..), ResourceRegistry
    , MeshResource, MeshFile(..), TextureResource
    , requestMeshResource, requestTextureResource
    , initialRegistry
    ) where

import           Yage.Prelude hiding (Index)
import           Yage.Lens

import           Data.Proxy
import           Data.Digest.XXHash
import qualified Data.Trie as T

import           Codec.Picture

import           Control.Monad.RWS.Strict
import qualified Data.Map.Strict as M

import           Yage.Rendering.Vertex
import           Yage.Rendering.Mesh

data MeshFile geo =
      OBJFile FilePath
    | YGMFile FilePath
    | NullResource (Proxy (Vertex geo)) -- currently no clue how to avoid this

type MeshResource geo  = Either (MeshFile geo) (Mesh geo)
type TextureResource = Either FilePath (String, DynamicImage)

data ResourceRegistry geo = ResourceRegistry
    { loadedMeshes   :: M.Map XXHash (Mesh geo)
    , loadedTextures :: T.Trie (String, DynamicImage)
    }

type YageResources geo = RWST (ResourceLoader geo) () (ResourceRegistry geo) IO



type MeshLoader geo = FilePath -> IO (Mesh geo)

type ResourceLoaderAccessor geo = ResourceLoader geo -> MeshLoader geo

data ResourceLoader geo = ResourceLoader
    { objLoader :: MeshLoader geo
    , ygmLoader :: MeshLoader geo
    }

runYageResources :: (MonadIO m) => ResourceLoader geo -> YageResources geo a -> ResourceRegistry geo -> m (a, ResourceRegistry geo)
runYageResources loader yr st = do
    (a, res, ()) <- io $ runRWST yr loader st
    return (a, res)


requestMeshResource :: MeshResource geo -> YageResources geo (MeshResource geo)
requestMeshResource (Left res) = Right <$> loadMeshFromFile res
requestMeshResource mesh       = return mesh


requestTextureResource :: TextureResource -> YageResources geo TextureResource
requestTextureResource (Left filepath) = Right <$> loadTextureFromFile filepath
requestTextureResource alreadyLoaded = return alreadyLoaded


loadTextureFromFile :: FilePath -> YageResources geo (String, DynamicImage)
loadTextureFromFile f = do
    let filepath = encodeUtf8 . fpToText $ f
        fpStr    = fpToString f
    registry <- get
    res <- maybe
            (load fpStr)
            return
            (registry^.textures.at filepath)

    put $ registry & textures.at filepath ?~ res
    return res
    where
    load path = io $ do
        eImg <- readImage path
        case eImg of
            Left err -> error err
            Right dyn -> return (path, dyn)



loadMeshFromFile :: MeshFile geo -> YageResources geo (Mesh geo)
loadMeshFromFile (YGMFile filepath) = loadMesh' filepath ygmLoader
loadMeshFromFile (OBJFile filepath) = loadMesh' filepath objLoader
loadMeshFromFile _ = error "Yage.Resources: invalid null resource"


loadMesh' :: FilePath -> ResourceLoaderAccessor geo -> YageResources geo (Mesh geo)
loadMesh' filepath loader = do
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


initialRegistry :: ResourceRegistry geo
initialRegistry = ResourceRegistry M.empty T.empty

meshes :: Lens' (ResourceRegistry geo) (M.Map XXHash (Mesh geo))
meshes = lens loadedMeshes (\r m -> r{ loadedMeshes = m })

textures :: Lens' (ResourceRegistry geo) (T.Trie (String, DynamicImage))
textures = lens loadedTextures (\r t -> r{ loadedTextures = t })

