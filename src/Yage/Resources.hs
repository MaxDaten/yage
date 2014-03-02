{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Resources where

import           Yage.Prelude
import           Yage.Lens

import           Data.Proxy
import           Data.Digest.XXHash

import           Control.Monad.RWS.Strict
import qualified Data.Map.Strict as M


import           Yage.Rendering.Types
import           Yage.Rendering.Vertex
import           Yage.Geometry.Formats.Obj (OBJ, parseOBJFile)
-- import           Yage.Geometry.Formats.Ygm (YGM, ygmFromFile)

data VertexResource geo =
      OBJResource FilePath (Proxy (Vertex geo))
    | YGMResource FilePath (Proxy (Vertex geo))


type VertexData geo = Either (VertexResource geo) (Mesh geo)

type ResourceRegistry geo = M.Map XXHash (Mesh geo)

type YageResources geo = RWST (ResourceLoader geo) () (ResourceRegistry geo) IO

data ResourceLoader geo = ResourceLoader
    { objLoader :: OBJ -> Mesh geo
    , ygmLoader :: FilePath -> IO (Mesh geo)
    }

runYageResources :: (MonadIO m) => ResourceLoader geo -> YageResources geo a -> ResourceRegistry geo -> m (a, ResourceRegistry geo)
runYageResources loader yr st = do
    (a, res, ()) <- io $ runRWST yr loader st
    return (a, res)


requestVertexData :: VertexData geo -> YageResources geo (VertexData geo)
requestVertexData (Left res)    = Right <$> loadVertexResource res
requestVertexData mesh          = return mesh


loadVertexResource :: VertexResource geo -> YageResources geo (Mesh geo)
loadVertexResource (YGMResource filepath _) = do
    xhash <- xxHash <$> readFile filepath
    registry <- get
    res <- maybe 
            load
            return
            (registry^.at xhash)
    
    put $ registry & at xhash ?~ res
    return res
    
    where
    load = do
        loader  <- asks ygmLoader
        io $ loader filepath

loadVertexResource (OBJResource filepath _) = do
    xhash <- xxHash <$> readFile filepath -- cache filepath -> hash (or memo?)
    registry <- get
    res <- maybe 
            load
            return
            (registry^.at xhash)
    
    put $ registry & at xhash ?~ res
    return res
    
    where
    load = do
        loader  <- asks objLoader
        obj     <- io $ parseOBJFile filepath
        return $ loader obj


initialRegistry :: ResourceRegistry geo
initialRegistry = M.empty