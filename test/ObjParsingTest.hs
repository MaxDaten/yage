{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import Test.Hspec

import Yage.Prelude hiding (group)
import Yage.Math
import Yage.Lens hiding (elements)


import qualified Data.Vector as V
import Data.Vinyl.Instances ()

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

    it "parses a cube with groups for each side" $ do
      parsedObj <- parseOBJFile $ "test" </> "res" </> "cube_groups.obj"
      parsedObj `shouldBe` cubeWithGroupsOBJ

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


faceParsing :: Spec
faceParsing = describe "face parsing" $ do
  it "parses a face with only geo indices" $
    testParser faceLine "f  1 2 3\n" `shouldBe` Right (Face $ References . singleton <$> [OBJVertexIndex 1, OBJVertexIndex 2, OBJVertexIndex 3])

  it "parses a face with geo, tex & normal indices" $
    testParser faceLine "f  1/2/3 2/3/4 3/4/5\n" `shouldBe` Right (Face [ References [ OBJVertexIndex 1, OBJTextureIndex 2, OBJNormalIndex 3 ]
                                                                        , References [ OBJVertexIndex 2, OBJTextureIndex 3, OBJNormalIndex 4 ]
                                                                        , References [ OBJVertexIndex 3, OBJTextureIndex 4, OBJNormalIndex 5 ]
                                                                        ])

  it "parses a face with geo, tex & normal indices with pending space" $
    testParser faceLine "f  1/2/3 2/3/4 3/4/5 \n" `shouldBe` Right (Face [ References [ OBJVertexIndex 1, OBJTextureIndex 2, OBJNormalIndex 3 ]
                                                                        , References [ OBJVertexIndex 2, OBJTextureIndex 3, OBJNormalIndex 4 ]
                                                                        , References [ OBJVertexIndex 3, OBJTextureIndex 4, OBJNormalIndex 5 ]
                                                                        ])

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
                   & groups.at "default"          ?~ (SmoothingGroups $ mempty & at 0 ?~ elems)
                   & comments                     .~ [ "File exported by ZBrush version 3.5"
                                                     , "www.zbrush.com"
                                                     , "Vertex Count 8844"
                                                     , "UV Vertex Count 35368"
                                                     , "Face Count 8842"
                                                     , "Auto scale x=4.472662 y=4.472662 z=4.472662"
                                                     , "Auto offset x=-0.022567 y=0.418160 z=0.309246"
                                                     ]
  where elems = (mempty & faces .~ V.singleton (Face  [ References [ OBJVertexIndex 1, OBJTextureIndex 1, OBJNormalIndex 1 ]
                                                      , References [ OBJVertexIndex 2, OBJTextureIndex 2, OBJNormalIndex 1 ]
                                                      , References [ OBJVertexIndex 3, OBJTextureIndex 4, OBJNormalIndex 1 ]
                                                      , References [ OBJVertexIndex 4, OBJTextureIndex 3, OBJNormalIndex 1 ]
                                                      ]))

cubeWithGroupsOBJ :: OBJ
cubeWithGroupsOBJ =
  mempty & vertexData.geometricVertices .~ V.fromList ( GeoVertex <$> [ V3 0 2 2, V3 0 0 2, V3 2 0 2, V3 2 2 2, V3 0 2 0, V3 0 0 0, V3 2 0 0, V3 2 2 0 ])
         & groups.at "cube"             ?~ ( SmoothingGroups $ mempty & at 1 ?~ cube    )
         & groups.at "front"            ?~ ( SmoothingGroups $ mempty & at 1 ?~ front   )
         & groups.at "back"             ?~ ( SmoothingGroups $ mempty & at 1 ?~ back    )
         & groups.at "right"            ?~ ( SmoothingGroups $ mempty & at 1 ?~ right   )
         & groups.at "top"              ?~ ( SmoothingGroups $ mempty & at 1 ?~ top     )
         & groups.at "left"             ?~ ( SmoothingGroups $ mempty & at 1 ?~ left    )
         & groups.at "bottom"           ?~ ( SmoothingGroups $ mempty & at 1 ?~ bottom  )
         & comments                     .~ ["from obj spec", "8 vertices", "6 elements"]
  where
  cube  = mconcat [ front, back, right, top, left, bottom ]
  front  = mempty & faces .~ V.singleton (Face [ References [ OBJVertexIndex 1 ], References [ OBJVertexIndex 2 ], References [ OBJVertexIndex 3 ], References [ OBJVertexIndex 4 ] ])
  back   = mempty & faces .~ V.singleton (Face [ References [ OBJVertexIndex 8 ], References [ OBJVertexIndex 7 ], References [ OBJVertexIndex 6 ], References [ OBJVertexIndex 5 ] ])
  right  = mempty & faces .~ V.singleton (Face [ References [ OBJVertexIndex 4 ], References [ OBJVertexIndex 3 ], References [ OBJVertexIndex 7 ], References [ OBJVertexIndex 8 ] ])
  top    = mempty & faces .~ V.singleton (Face [ References [ OBJVertexIndex 5 ], References [ OBJVertexIndex 1 ], References [ OBJVertexIndex 4 ], References [ OBJVertexIndex 8 ] ])
  left   = mempty & faces .~ V.singleton (Face [ References [ OBJVertexIndex 5 ], References [ OBJVertexIndex 6 ], References [ OBJVertexIndex 2 ], References [ OBJVertexIndex 1 ] ])
  bottom = mempty & faces .~ V.singleton (Face [ References [ OBJVertexIndex 2 ], References [ OBJVertexIndex 6 ], References [ OBJVertexIndex 7 ], References [ OBJVertexIndex 3 ] ])
