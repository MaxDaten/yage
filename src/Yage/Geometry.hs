{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}
module Yage.Geometry
    ( NormalSmoothness(..), Primitive(..)
    , module Vertex
    , module Geometry
    , module Data.Vinyl
    , module Yage.Geometry
    ) where

import Yage.Prelude

import Data.Vinyl
import Yage.Geometry.Vertex             as Vertex hiding (P3, P3N3)
import "yage-geometry" Yage.Geometry    as Geometry
import Yage.Primitives                  (Primitive(..), calculateNormals)
import Graphics.Rendering.OpenGL        (GLfloat)

import qualified Data.Vector as V

-- deriving instance (Eq a) => Eq (Identity a)

type YPosition3 = Position3 "vposition" GLfloat
type YPosition2 = Position2 "vposition" GLfloat
type YNormal3   = Normal3   "vnormal"   GLfloat
type YTexture2  = Texture2  "vtexture"  GLfloat 
type YColor4    = Color4    "vcolor"    GLfloat

type P3N3T2C4 =  [ YPosition3, YNormal3, YTexture2, YColor4 ]
type P3N3T2   =  [ YPosition3, YNormal3, YTexture2 ]
type P2T2C4   =  [ YPosition2, YTexture2, YColor4 ]
type P2T2     =  [ YPosition2, YTexture2 ]
type P3N3     =  [ YPosition3, YNormal3 ]
type P3T2     =  [ YPosition3, YTexture2 ]
type P2       = '[ YPosition2 ]
type P3       = '[ YPosition3 ]


normalCalculator :: NormalSmoothness -> Primitive (Vertex P3) -> Primitive (Vertex P3N3)
normalCalculator = calculateNormals (position3 :: YPosition3) normal3

loadObj :: OBJ -> Geometry (Vertex P3N3)
loadObj obj = 
    let geo     = geoElements $ geometryFromOBJ obj (position3 :: YPosition3)
        flatVec = V.concatMap (V.fromList . vertices . triangles . faceNorm) geo
    in Geometry flatVec  


faceNorm :: Face (Vertex P3) -> Face (Vertex P3N3)
faceNorm = addFaceNormal (position3 :: YPosition3) normal3

{--
position3 :: Position3
position3 = Field

normal3 :: Normal3 
normal3 = Field

texture2 :: Texture2
texture2 = Field

color4 :: Color4
color4 = Field

position2 :: Position2
position2 = Field
--}

