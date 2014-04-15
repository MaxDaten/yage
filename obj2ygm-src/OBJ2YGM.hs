module Main where

import Yage.Prelude hiding (head)

import Data.List hiding ((++))
import Data.Proxy


import Yage.Geometry.Vertex
import Yage.Geometry
import qualified Yage.Geometry.Formats.Ygm as YGM
import Yage.Geometry.Formats.Ygm ()
import qualified Yage.Geometry.Formats.Obj as OBJ

type OBJVertex = P3T2 "pos" "tex" Float
type YGMVertex = P3T2NT3 "pos" "tex" "norm" "tan" Float

main :: IO ()
main = do
    importFile <- fpFromText . head <$> getArgs

    print $ "import...: " ++ show importFile
    (pGeo, tGeo) <- OBJ.geometryFromOBJFile importFile
    let name        = fpToText . basename $ importFile
        exportFile  = basename importFile <.> "ygm"
        ntGeo       = genSmoothings pGeo tGeo
        ygm         = YGM.YGM name (packGeos3 YGM.vertexFormat pGeo tGeo ntGeo) :: YGM.YGM YGMVertex

    print $ "...export: " ++ show exportFile
    YGM.ygmToFile exportFile ygm
    print ygm

    print $ "check: " ++ show exportFile
    fileCheck <- YGM.ygmFromFile exportFile (Proxy::Proxy YGMVertex)
    print fileCheck
    --print $ format "file correct: {0}" [show $ fileCheck == ygm]
