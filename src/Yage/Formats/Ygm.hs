{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Yage.Formats.Ygm
    ( module Yage.Formats.Ygm
    ) where

import           Yage.Lens
import           Yage.Prelude


import           Data.Binary
import           Data.Map                    (keys)
import           Data.Text.Binary            as Bin ()
import           Data.Vinyl.Instances        ()

import           Codec.Compression.GZip
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy        as B

import           Linear
import           Yage.Geometry

-- yage geometry model
type InternalFormat a = Y'P3TX2TN a
type YGMFormat = InternalFormat Float
data YGM = YGM
    { ygmName   :: Text
    , ygmModels :: Map Text (TriGeo (Vertex YGMFormat))
    } deriving ( Eq, Typeable, Generic )


ygmToFile :: FilePath -> YGM -> IO ()
ygmToFile name = B.writeFile (fpToString name) . compress . encode

ygmFromFile :: FilePath -> IO YGM
ygmFromFile path = do
    !ygm <- ({-# SCC "decode" #-} decode) .
            ({-# SCC "decompress" #-} decompress) <$>
             {-# SCC "readFile" #-} (B.readFile (fpToString path))
    return ygm { ygmModels = {-# SCC "ygmModels" #-} ygmModels ygm `using` parTraversable ({-# SCC "deepsec" #-} rdeepseq) }

internalFormat :: ( Real a, Fractional a, Fractional b ) =>
               Pos a ->
               Tex a ->
               TBN a ->
               Vertex (InternalFormat b)
internalFormat pos tex tangentBasis@(V3 t _b n) =
    position3 =: ( realToFrac <$> pos )      <+>
    texture2  =: ( realToFrac <$> tex )      <+>
    tangentX  =: ( realToFrac <$> t )        <+>
    tangentZ  =: ( realToFrac <$> normal )
    where
    normal    = vector n & _w .~ basisSign
    basisSign = if det33 tangentBasis < 0 then -1.0 else 1.0


sameModels :: YGM -> YGM -> Bool
sameModels a b = (ygmModels a) == (ygmModels b)

instance Show YGM where
    show YGM{ygmName, ygmModels} = unpack $ format "YGM {name = {}, groups={}}" (Shown ygmName, Shown $ keys ygmModels)

instance Binary YGM

instance NFData YGM where rnf = genericRnf
