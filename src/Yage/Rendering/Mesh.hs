{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE ExistentialQuantification   #-}
{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE FlexibleContexts            #-}

module Yage.Rendering.Mesh
  ( module Yage.Rendering.Mesh
  , module E
  ) where

import           Yage.Prelude                        hiding (Index, toList)
import           Yage.Lens                           hiding (snoc)

import qualified Data.Vector.Storable                as VS
import qualified Data.Vector                         as V
import           Data.Foldable                       (toList)
import           Data.List                           (mapAccumL)
import qualified Data.Digest.XXHash                  as XH
import qualified Data.Vector.Storable.ByteString     as BS

import           Data.Bits
import Yage.Geometry
import Yage.Geometry.Elements as E (Triangle(..))
---------------------------------------------------------------------------------------------------



type MeshHash = XH.XXHash
type MeshId   = ByteString


-- TODO: Materials to component
data MeshComponent = MeshComponent
  { _componentId             :: MeshId
  , _componentIndexBuffer    :: (VS.Vector Int)
  , _componentHash           :: MeshHash
  } deriving ( Typeable, Generic )


-- TODO: smart vertex book-keeping?
data Mesh v = Mesh
    { _meshId             :: MeshId
    , _meshVertexBuffer   :: [(VS.Vector v)]
    , _meshComponents     :: (Map MeshId MeshComponent)
    , _meshHash           :: MeshHash
    } deriving ( Typeable, Generic )

makeLenses ''MeshComponent
makeLenses ''Mesh


vertexCount :: Storable v => Getter (Mesh v) Int
vertexCount = meshVertexBuffer.to (sum . map VS.length)
{-# INLINE vertexCount #-}


componentCount :: Getter (Mesh v) Int
componentCount = meshComponents.to (lengthOf traverse)
{-# INLINE componentCount #-}

indexCount :: Getter MeshComponent Int
indexCount = componentIndexBuffer.to (VS.length)
{-# INLINE indexCount #-}

meshComponentsHash :: Getter (Mesh v) MeshHash
meshComponentsHash = meshComponents.to compHashes where
    compHashes compMap = XH.xxHash . pack $ concatMap octets $ compMap^..traverse.componentHash
{-# INLINE meshComponentsHash #-}


-- | concat of the indices of all `MeshComponent`s
concatedMeshIndices :: Getter (Mesh v) (VS.Vector Int)
concatedMeshIndices = meshComponents.to concatIndices where
    concatIndices compMap = VS.concat $ compMap^..traverse.componentIndexBuffer
{-# INLINE concatedMeshIndices #-}


meshIndexRanges :: Getter (Mesh v) [(Int, Int)]
meshIndexRanges = meshComponents.to ranges where
    ranges compMap = snd $ mapAccumL (\pos len -> (pos+len, (pos, pos+len-1))) 0 $
                        filter (>0) $ compMap^..traverse.componentIndexBuffer.to VS.length
{-# INLINE meshIndexRanges #-}

---------------------------------------------------------------------------------------------------


meshVertices :: Storable v => Lens' (Mesh v) (VS.Vector v)
meshVertices = lens getter setter where
  getter mesh = VS.concat $ mesh^.meshVertexBuffer
  setter mesh verts = mesh{ _meshVertexBuffer = [verts] }
{-# INLINE meshVertices #-}


mkFromVerticesF :: ( Storable v, Foldable f ) => MeshId -> f v -> Mesh v
mkFromVerticesF ident = mkFromVertices ident . VS.fromList . toList


-- | mesh with single component with trivial indices
mkFromVertices :: Storable v => MeshId -> VS.Vector v -> Mesh v
mkFromVertices ident verts =
  emptyMesh & meshId               .~ ident
            & meshVertices         .~ verts
            & meshComponents.at "" ?~ (makeComponent "" ( VS.generate (VS.length verts) id ))



makeComponent :: MeshId -> VS.Vector Int -> MeshComponent
makeComponent ident indices = MeshComponent ident indices (hashVector indices)
{-# INLINE makeComponent #-}


componentIndices :: Lens' MeshComponent (VS.Vector Int)
componentIndices = lens _componentIndexBuffer setter where
  setter comp idxs = comp & componentIndexBuffer .~ idxs
                          & componentHash .~ (hashVector idxs)
{-# INLINE componentIndices #-}

-- | builds a mesh from geometry

-- preserving the elements but flattens the surfaces
-- a component for the indices is created at the root ("")
meshFromTriGeo :: (Storable v) => MeshId -> TriGeo v -> Mesh v
meshFromTriGeo ident geo@Geometry{..} =
    emptyMesh & meshId                .~ ident
              & meshVertices          .~ VS.convert _geoVertices
              & meshComponents.at ""  ?~ makeComponent "" (geo^.flattenIndices)


-- | unified empty mesh with "" identifier
emptyMesh :: Storable v => Mesh v
emptyMesh = Mesh "" [] mempty 0
{-# INLINE emptyMesh #-}


-- | appends a new component to a mesh
--
-- keeps id
-- hash is recalculated
-- component indices are recalculated
-- component id is prepended with meshId and a dot `.` (meshId.componentId)
-- component indices aren't bound-checked against the given vertices
appendComponent :: Storable v => Mesh v -> ( MeshComponent, VS.Vector v ) -> Mesh v
appendComponent mesh (comp, verts) = {-# SCC appendComponent #-}
    mesh & meshVertexBuffer                      <>~ [verts]
         & meshComponents.at (comp^.componentId) ?~ componentToAdd
    where
    componentToAdd = {-# SCC "appendComponent.componentToAdd" #-}
        comp & componentIndices     %~ VS.map (+ VS.length (mesh^.meshVertices) )
{-# INLINE appendComponent #-}


appendGeometry :: ( Storable v ) => Mesh v -> (MeshId, TriGeo v) -> Mesh v
appendGeometry mesh (ident, geo) = {-# SCC appendGeometry #-}
    let idxs  = geo^.flattenIndices
        verts = {-# SCC convert #-} VS.convert $ geo^.geoVertices
    in mesh `appendComponent` (makeComponent ident idxs, verts)
{-# INLINE appendGeometry #-}

{--
Utilities
--}

-- | colapse surfaces
flattenIndices :: Getter (TriGeo v) (VS.Vector Int)
flattenIndices = {-# SCC flattenIndices #-}
    to $ VS.concatMap (VS.fromList . toList) . VS.convert . flatten . _geoSurfaces
    where flatten = V.foldl' (\accum (GeoSurface surf) -> accum V.++ surf) V.empty
{-# INLINE flattenIndices #-}


hashVector :: Storable a => VS.Vector a -> XH.XXHash
hashVector = {-# SCC hashVector #-} XH.xxHash' . BS.vectorToByteString
{-# INLINE hashVector #-}

-- stolen from http://www.haskell.org/pipermail/beginners/2010-October/005571.html
octets :: Word32 -> [Word8]
octets w =
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]
{-# INLINE octets #-}

instance (Storable v) => Show (Mesh v) where
    show m@Mesh{..} = show $
      format "Mesh { id = {}, #vertexBuffer = {}, meshHash = {}, components = {} }"
               ( Shown _meshId
               , Shown $ m^.vertexCount
               --, Shown $ _meshVertexBuffer
               , hex _meshHash
               , Shown $ _meshComponents^..traverse
               )

instance Show MeshComponent where
  show MeshComponent{..} = show $
    format "MeshComponent { id = {}, #indexBuffer = {}-{}, hash = {} }"
      ( Shown _componentId
      , Shown $ VS.length _componentIndexBuffer
      , Shown $ _componentIndexBuffer
      , hex _componentHash
      )

instance Eq (Mesh v) where
    a == b = _meshId a == _meshId b


instance Ord (Mesh v) where
    compare a b = compare (_meshId a) (_meshId b)

instance NFData v => NFData (Mesh v) where rnf = genericRnf
instance NFData MeshComponent        where rnf = genericRnf

