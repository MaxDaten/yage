{-# LANGUAGE PackageImports #-}
module Main where

import Yage.Prelude hiding (head)

import Data.List hiding ((++))
import Data.Proxy


import Yage.Geometry.Vertex
import "yage" Yage.Geometry
import qualified Yage.Formats.Ygm as YGM
import Yage.Formats.Ygm ()
import qualified Yage.Formats.Obj as OBJ

type OBJVertex = P3T2 "pos" "tex" Float
type YGMVertex = P3T2NT3 "pos" "tex" "norm" "tan" Float

main :: IO ()
main = do
    importFile <- fpFromText . head <$> getArgs

    print $ "import...: " ++ show importFile
    (pGeo, tGeo) <- OBJ.geometryFromOBJFile importFile
    let name        = fpToText . basename $ importFile
        exportFile  = basename importFile <.> "ygm"
        tbnGeo      = calcTangentSpaces pGeo tGeo
        ygm         = YGM.YGM name $ packGeos YGM.internalFormat pGeo tGeo tbnGeo

    print $ "...export: " ++ show exportFile
    YGM.ygmToFile exportFile ygm
    print ygm

    fileCheck <- YGM.ygmFromFile exportFile
    print $ format "file correct: {0}" [show $ fileCheck == ygm]
