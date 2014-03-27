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
    , VertexData, VertexResource(..), TextureResource
    , requestVertexData, requestTextureResource
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

data VertexResource geo =
      OBJResource FilePath
    | YGMResource FilePath
    | NullResource (Proxy (Vertex geo)) -- currently no clue how to avoid this

type VertexData geo  = Either (VertexResource geo) (TriMesh geo)
type TextureResource = Either FilePath (String, DynamicImage)

data ResourceRegistry geo = ResourceRegistry
    { loadedMeshes   :: M.Map XXHash (TriMesh geo)
    , loadedTextures :: T.Trie (String, DynamicImage)
    }

type YageResources geo = RWST (ResourceLoader geo) () (ResourceRegistry geo) IO

data ResourceLoader geo = ResourceLoader
    { objLoader :: FilePath -> IO (TriMesh geo)
    , ygmLoader :: FilePath -> IO (TriMesh geo)
    }

runYageResources :: (MonadIO m) => ResourceLoader geo -> YageResources geo a -> ResourceRegistry geo -> m (a, ResourceRegistry geo)
runYageResources loader yr st = do
    (a, res, ()) <- io $ runRWST yr loader st
    return (a, res)


requestVertexData :: VertexData geo -> YageResources geo (VertexData geo)
requestVertexData (Left res)    = Right <$> loadVertexResource res
requestVertexData mesh          = return mesh


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



loadVertexResource :: VertexResource geo -> YageResources geo (TriMesh geo)
loadVertexResource (YGMResource filepath) = do
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
        loader  <- asks ygmLoader
        io $ loader filepath

loadVertexResource (OBJResource filepath) = do
    xhash <- xxHash <$> readFile filepath -- cache filepath -> hash (or memo?)
    registry <- get
    res <- maybe 
            load
            return
            (registry^.meshes.at xhash)
    
    put $ registry & meshes.at xhash ?~ res
    return res
    
    where
    load = do
        loader  <- asks objLoader
        io $ loader filepath

loadVertexResource _ = error "Yage.Resources: invalid null resource"


initialRegistry :: ResourceRegistry geo
initialRegistry = ResourceRegistry M.empty T.empty

meshes :: Lens' (ResourceRegistry geo) (M.Map XXHash (TriMesh geo))
meshes = lens loadedMeshes (\r m -> r{ loadedMeshes = m })

textures :: Lens' (ResourceRegistry geo) (T.Trie (String, DynamicImage))
textures = lens loadedTextures (\r t -> r{ loadedTextures = t })

