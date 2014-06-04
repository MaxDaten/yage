{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DeriveDataTypeable     #-}
module Yage.Formats.Ygm
    ( module Yage.Formats.Ygm
    ) where

import Yage.Prelude
import Yage.Lens

import Data.Binary
import Data.Text.Binary as Bin ()
import Data.Vinyl.Instances ()
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy as B

import Yage.Geometry
import Linear

-- yage geometry model
type InternalFormat a = Y'P3TX2TN a
type YGMFormat = InternalFormat Float
data YGM = YGM
    { ygmName  :: Text
    , ygmModel :: TriGeo (Vertex YGMFormat)
    } deriving ( Eq, Typeable, Generic )


ygmToFile :: FilePath -> YGM -> IO ()
ygmToFile name = B.writeFile (fpToString name) . compress . encode

ygmFromFile :: FilePath -> IO YGM
ygmFromFile path = decode . decompress <$> (B.readFile $ fpToString path)


internalFormat :: ( Real a, Fractional a, Fractional b ) => 
               Pos a ->
               Tex a ->
               TBN a ->
               Vertex (InternalFormat b)
internalFormat pos tex tangentBasis@(V3 t _b n) =
    yposition3 =: ( realToFrac <$> pos )      <+>
    ytexture2  =: ( realToFrac <$> tex )      <+>
    ytangentX  =: ( realToFrac <$> t )        <+>
    ytangentZ  =: ( realToFrac <$> normal )
    where
    normal    = vector n & _w .~ basisSign
    basisSign = if det33 tangentBasis < 0 then -1.0 else 1.0


sameModel :: YGM -> YGM -> Bool
sameModel a b = (ygmModel a) == (ygmModel b) 

instance Show YGM where
    show YGM{ygmName} = format "YGM {name = {0}}" [show ygmName]

instance Binary YGM
