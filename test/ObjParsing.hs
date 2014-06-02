{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main (main, spec) where

import Test.Hspec

import Yage.Prelude
import Yage.Math
import Yage.Lens hiding (elements)

import qualified Data.Vector as V
import Data.Vinyl.Instances ()

import Yage.Geometry
import Yage.Formats.Obj



main :: IO ()
main = hspec spec

-- Tests

spec :: Spec
spec = do
  describe "parse OBJ files" $ do
    it "parses a simple square" $ do
      parsedObj <- parseOBJFile $ "test" </> "res" </> "square.obj"
      parsedObj `shouldBe` squareOBJ

    it "parses a simple square into position Geometry" $ do
      (pos, _tex) <- geometryFromOBJFile $ "test" </> "res" </> "square.obj"
      pos `shouldBe` squarePos

    it "parses a simple square into texture Geometry" $ do
      (_pos, tex) <- geometryFromOBJFile $ "test" </> "res" </> "square.obj"
      tex `shouldBe` squareTex


-- fixtures

squareOBJ :: OBJ
squareOBJ = mempty & vertexData.geometricVertices .~ V.fromList [ V3 0 2 0, V3 0 0 0, V3 2 0 0, V3 2 2 0 ]
                   & vertexData.vertexNormals     .~ V.fromList [ V3 0 0 1 ]
                   & vertexData.textureVertices   .~ V.fromList [ V2 0 0  , V2 0 1  , V2 1 0  , V2 1 1 ]
                   & elements.faces .~ V.singleton [[ VertexIndex 1, TextureIndex 1, NormalIndex 1 ]
                                                   ,[ VertexIndex 2, TextureIndex 2, NormalIndex 1 ]
                                                   ,[ VertexIndex 3, TextureIndex 4, NormalIndex 1 ]
                                                   ,[ VertexIndex 4, TextureIndex 3, NormalIndex 1 ]
                                                   ]


squarePos :: TriGeo (V3 Float)
squarePos = Geometry
  { geoVertices = V.fromList [ V3 0 2 0
                             , V3 0 0 0
                             , V3 2 0 0
                             , V3 2 2 0
                             ]
  , geoElements = V.fromList [Triangle 0 1 2, Triangle 0 2 3]
  }


squareTex :: TriGeo (V2 Float)
squareTex = Geometry
  { geoVertices = V.fromList [ V2 0 0
                             , V2 0 1
                             , V2 1 0
                             , V2 1 1
                             ]
  , geoElements = V.fromList [Triangle 0 1 3, Triangle 0 3 2]
  }
