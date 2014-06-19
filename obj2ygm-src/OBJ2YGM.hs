{-# LANGUAGE PackageImports #-}
module Main where

import Yage.Prelude hiding (head)
import Yage.Lens hiding ((<.>))

import Data.List hiding ((++), zipWith, elem)
import Data.Proxy
import qualified Data.Map as M


import Yage.Geometry.Vertex
import "yage" Yage.Geometry
import qualified Yage.Formats.Ygm as YGM
import Yage.Formats.Ygm ()
import qualified Yage.Formats.Obj as OBJ

main :: IO ()
main = do
    importFile <- fpFromText . head <$> getArgs
    let subSelection = []

    print $ "import...: " ++ show importFile
    OBJ.GeometryGroup geoMap <- printIOTime $ OBJ.geometryFromOBJFile importFile
    let name        = fpToText . basename $ importFile
        exportFile  = basename importFile <.> "ygm"
        
        geos        = M.filterWithKey (isSelected subSelection) . M.mapKeys decodeUtf8 $ geoMap
        tbnGeos     = over traverse (uncurry calcTangentSpaces) geos
        packed      = mergeGeos geos tbnGeos
        
        ygm         = YGM.YGM name packed

    print $ "...export: " ++ show exportFile
    -- print ygm
    printIOTime $ YGM.ygmToFile exportFile ygm

    fileCheck <- printIOTime $ YGM.ygmFromFile exportFile
    print $ format "file correct: {0}" [show $ fileCheck == ygm]
    
    where

    mergeGeos = M.mergeWithKey (\_key (pos,tex) tbn -> Just $ packGeos YGM.internalFormat pos tex tbn) (const M.empty) (const M.empty)

    isSelected []        _ _   = True
    isSelected selection key _ = key `elem` selection

