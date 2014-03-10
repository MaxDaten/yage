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

import                  Data.Vinyl
import                  Yage.Geometry.Vertex             as Vertex hiding (P3, P3N3, P3T2, P3T2N3)

import "yage-geometry"  Yage.Geometry                    as Geometry
import                  Yage.Primitives                  (Primitive(..), calculateNormals)
import                  Graphics.Rendering.OpenGL        (GLfloat)


-- deriving instance (Eq a) => Eq (Identity a)

type YPosition3 = Position3 "vposition" GLfloat
type YPosition2 = Position2 "vposition" GLfloat
type YNormal3   = Normal3   "vnormal"   GLfloat
type YTangent3  = Tangent3  "vtangent"  GLfloat
type YTexture2  = Texture2  "vtexture"  GLfloat 
type YColor4    = Color4    "vcolor"    GLfloat

type P3N3TX2C4 =  [ YPosition3, YNormal3, YTexture2, YColor4 ]
type P3TX2N3   =  [ YPosition3, YTexture2, YNormal3 ]
type P2TX2C4   =  [ YPosition2, YTexture2, YColor4 ]
type P2TX2     =  [ YPosition2, YTexture2 ]
type P3N3      =  [ YPosition3, YNormal3 ]
type P3TX2NT3  =  [ YPosition3, YTexture2, YNormal3, YTangent3 ]
type P3TX2     =  [ YPosition3, YTexture2 ]
type P2        = '[ YPosition2 ]
type P3        = '[ YPosition3 ]


normalCalculator :: NormalSmoothness -> Primitive (Vertex P3) -> Primitive (Vertex P3N3)
normalCalculator = calculateNormals (position3 :: YPosition3) normal3

faceNorm :: Face (Vertex P3) -> Face (Vertex P3N3)
faceNorm = calcFaceNormal (position3 :: YPosition3) normal3

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
