{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import Test.Hspec

import Yage.Prelude hiding (group)
import Yage.Math
import Yage.Lens hiding (elements)


import qualified Data.Vector as V
import Data.Vinyl.Instances ()

import qualified Data.ByteString.Char8 as B (pack)
import Yage.Geometry hiding (Face, Line)
import Yage.Formats.Obj
import Yage.Formats.Obj.Parser

import Data.Attoparsec.ByteString (Parser, parseOnly, endOfInput)



main :: IO ()
main = hspec $ do
  fileElementSpec


  describe "parse OBJ files" $ do
    it "parses a simple square" $ do
      parsedObj <- parseOBJFile $ "test" </> "res" </> "square.obj"
      parsedObj `shouldBe` squareOBJ
  
{--

    it "parses a simple square into position Geometry" $ do
      (pos, _tex) <- geometryFromOBJFile $ "test" </> "res" </> "square.obj"
      pos `shouldBe` squarePos

    it "parses a simple square into texture Geometry" $ do
      (_pos, tex) <- geometryFromOBJFile $ "test" </> "res" </> "square.obj"
      tex `shouldBe` squareTex
--}
fileElementSpec :: Spec
fileElementSpec = do
  objFileItems

  vertexDataParsing
  
  faceParsing

  lineParsing

  groupParsing

  smoothingParsing

vertexDataParsing :: Spec
vertexDataParsing = describe "vertex data parsing" $ do
  it "parses a geometric vertex" $
    testParser geovertex "v     1.2 2.3 4.5" `shouldBe` Right (GeoVertex $ V3 1.2 2.3 4.5)
  
  it "parses a geometric vertex line with pending trash" $
    testParser geovertexLine "v     1.2 2.3 4.5 this is a 3d vertex\n" `shouldBe` Right (GeoVertex $ V3 1.2 2.3 4.5)
  

  it "parses a normal vertex" $
    testParser normalvertex "vn     1.2 2.3 4.5" `shouldBe` Right (NormalVertex $ V3 1.2 2.3 4.5)
  
  it "parses a normal vertex line with pendig trash" $
    testParser normalvertexLine "vn     1.2 2.3 4.5 this is a normal\n" `shouldBe` Right (NormalVertex $ V3 1.2 2.3 4.5)


  it "parses a texture vertex" $
    testParser texvertex "vt      1.2 2.3" `shouldBe` Right (TextureVertex $ V2 1.2 2.3)

  it "parses a texture vertex line with pendig trash" $
    testParser texvertexLine "vt      1.2 2.3    this is a texture coord\n" `shouldBe` Right (TextureVertex $ V2 1.2 2.3)


  it "parses VertexData" $
    testParser objVertexData testVertexDataStr `shouldBe` Right testVertexData


faceParsing :: Spec
faceParsing = describe "face parsing" $ do
  it "parses a face with only geo indices" $
    testParser faceLine "f  1 2 3\n" `shouldBe` Right (Face $ References . singleton <$> [OBJVertexIndex 1, OBJVertexIndex 2, OBJVertexIndex 3])

  it "parses a face with geo, tex & normal indices" $
    testParser faceLine "f  1/2/3 2/3/4 3/4/5\n" `shouldBe` Right (Face [ References [ OBJVertexIndex 1, OBJTextureIndex 2, OBJNormalIndex 3 ]
                                                                        , References [ OBJVertexIndex 2, OBJTextureIndex 3, OBJNormalIndex 4 ]
                                                                        , References [ OBJVertexIndex 3, OBJTextureIndex 4, OBJNormalIndex 5 ]
                                                                        ])
  it "parses a face block" $
    testParser objElements testFaceBlockStr `shouldBe` Right testFaceBlock


lineParsing :: Spec
lineParsing = describe "line parsing" $ do
  it "parses a line with only geo indices" $
    testParser lineLine "l 1 2\n" `shouldBe` Right (Line $ References . singleton <$> [OBJVertexIndex 1, OBJVertexIndex 2])

  it "parses a line with geo & tex indices" $
    testParser lineLine "l 1/2 2/3\n" `shouldBe` Right (Line [ References [ OBJVertexIndex 1, OBJTextureIndex 2 ]
                                                             , References [ OBJVertexIndex 2, OBJTextureIndex 3 ]
                                                             ])

groupParsing :: Spec
groupParsing = describe "group parsing" $ do
  it "parses a single group name" $ do
    testParser groupLine "g GroupName\n" `shouldBe` Right ([Group "GroupName"])

  it "parses a multiple group names" $ do
    testParser groupLine "g GroupName01 GroupName_01  \n" `shouldBe` Right ([Group "GroupName01", Group "GroupName_01"])


smoothingParsing :: Spec
smoothingParsing = describe "smoothing group parsing" $ do
  it "parses a numeric smoothing group" $
    testParser smoothingGroupLine "s 10\n" `shouldBe` (Right (SmoothingGroup 10))

  it "parses the 'off' smoothing group token" $
    testParser smoothingGroupLine "s off\n" `shouldBe` (Right (SmoothingGroup 0))


objFileItems :: Spec
objFileItems = do
  describe "vector parsing" $ do
    it "parses a V3" $
      testParser v3 "1.2   2.3   4.5" `shouldBe` Right (V3 1.2 2.3 4.5)

    it "parses a V2" $
      testParser v2 "1.2   2.3" `shouldBe` Right (V2 1.2 2.3)

  describe "comment parsing" $ do
    it "parses a comment line" $
      testParser commentLine "#   a comment\n" `shouldBe` Right "a comment"
    
    it "parses a comment line without leading space" $
      testParser commentLine "#x\n" `shouldBe` Right "x"
    
    it "parses an comment line with leading tab" $
      testParser commentLine "#\tx\n" `shouldBe` Right "x"

    it "parses an empty comment line without spaces" $
      testParser commentLine "#\n" `shouldBe` Right ""

    it "parses an empty comment line" $
      testParser commentLine "#  \n" `shouldBe` Right ""


testParser :: Parser a -> ByteString -> Either String a
testParser parser = parseOnly (parser <* endOfInput)

-- fixtures

squareOBJ :: OBJ
squareOBJ = mempty & vertexData.geometricVertices .~ V.fromList ( GeoVertex     <$> [ V3 0 2 0, V3 0 0 0, V3 2 0 0, V3 2 2 0 ] )
                   & vertexData.vertexNormals     .~ V.fromList ( NormalVertex  <$> [ V3 0 0 1 ] )
                   & vertexData.textureVertices   .~ V.fromList ( TextureVertex <$> [ V2 0 0  , V2 0 1  , V2 1 0  , V2 1 1 ] )
                   & elements.faces .~ V.singleton (Face [ References [ OBJVertexIndex 1, OBJTextureIndex 1, OBJNormalIndex 1 ]
                                                         , References [ OBJVertexIndex 2, OBJTextureIndex 2, OBJNormalIndex 1 ]
                                                         , References [ OBJVertexIndex 3, OBJTextureIndex 4, OBJNormalIndex 1 ]
                                                         , References [ OBJVertexIndex 4, OBJTextureIndex 3, OBJNormalIndex 1 ]
                                                         ])
                   & comments                     .~ [ "File exported by ZBrush version 3.5"
                                                     , "www.zbrush.com"
                                                     , "Vertex Count 8844"
                                                     , "UV Vertex Count 35368"
                                                     , "Face Count 8842"
                                                     , "Auto scale x=4.472662 y=4.472662 z=4.472662"
                                                     , "Auto offset x=-0.022567 y=0.418160 z=0.309246"
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


testVertexData :: OBJVertexData
testVertexData = 
  mempty & geometricVertices .~ V.fromList ( GeoVertex     <$> [ V3 0 0 0, V3 0 0 1, V3 1 1 1 ] )
         & vertexNormals     .~ V.fromList ( NormalVertex  <$> [ V3 0 0 1, V3 1 0 0, V3 (-1) 0 0 ] )
         & textureVertices   .~ V.fromList ( TextureVertex <$> [ V2 1 0  , V2 1 1 ] )


testVertexDataStr :: ByteString
testVertexDataStr = B.pack $ unlines $
  [ "v  0.0  0.0  0.0"
  , "v  0.0  0.0  1.0"
  , "v  1.0  1.0  1.0"
  , ""
  , "vn  0.0  0.0  1.0"
  , "vn  1.0  0.0  0.0"
  , "vn -1.0  0.0  0.0"
  , ""
  , ""
  , "vt 1.0 0.0"
  , "vt 1.0 1.0" ]


testFaceBlock :: OBJElements
testFaceBlock = mempty & faces .~ fs where
  fs = V.fromList $ Face <$> refs
  refs :: [[References]]
  refs =  fmap References <$> idxs
  idxs :: [[[OBJIndex]]]
  idxs = [ [ [ OBJVertexIndex 1, OBJTextureIndex 1, OBJNormalIndex 1 ], [ OBJVertexIndex 2, OBJTextureIndex 2, OBJNormalIndex 1 ], [ OBJVertexIndex 3, OBJTextureIndex 4, OBJNormalIndex 1 ] ]
         , [ [ OBJVertexIndex 5, OBJTextureIndex 4, OBJNormalIndex 2 ], [ OBJVertexIndex 6, OBJTextureIndex 2, OBJNormalIndex 2 ], [ OBJVertexIndex 7, OBJTextureIndex 1, OBJNormalIndex 2 ] ]
         , [ [ OBJVertexIndex 6, OBJTextureIndex 4, OBJNormalIndex 2 ], [ OBJVertexIndex 1, OBJTextureIndex 2, OBJNormalIndex 2 ], [ OBJVertexIndex 4, OBJTextureIndex 1, OBJNormalIndex 2 ] ]
         , [ [ OBJVertexIndex 2, OBJTextureIndex 4, OBJNormalIndex 2 ], [ OBJVertexIndex 1, OBJTextureIndex 2, OBJNormalIndex 2 ], [ OBJVertexIndex 5, OBJTextureIndex 1, OBJNormalIndex 2 ] ]
         ]

testFaceBlockStr :: ByteString
testFaceBlockStr = B.pack $ unlines $
  [ "f 1/1/1 2/2/1 3/4/1"
  , "f 5/4/2 6/2/2 7/1/2"
  , ""
  , "f 6/4/2 1/2/2 4/1/2"
  , ""
  , "f 2/4/2 1/2/2 5/1/2" ]
