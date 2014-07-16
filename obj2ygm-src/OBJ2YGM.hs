{-# LANGUAGE PackageImports #-}
module Main where

import Yage.Prelude hiding (head)
import Yage.Lens hiding ((<.>))

import Data.List hiding ((++), zipWith, elem)
import Data.Proxy
import qualified Data.Map as M


import "yage" Yage.Geometry
import qualified Yage.Formats.Ygm as YGM
import Yage.Formats.Ygm ()
import qualified Yage.Formats.Obj as OBJ

main :: IO ()
main = do
    importFile   <- fpFromText . head <$> getArgs
    subSelection <- tail <$> getArgs

    print $ "import... (lazy):" ++ show importFile
    OBJ.GeometryGroup geoMap <- printIOTime $! OBJ.geometryFromOBJFile importFile
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
    printTF "file correct: {0}" (Only $ Shown $ fileCheck == ygm)
    
    where

    mergeGeos = M.mergeWithKey (\_key (pos,tex) tbn -> Just $ packGeos YGM.internalFormat pos tex tbn) (const M.empty) (const M.empty)

    isSelected []        _ _   = True
    isSelected selection key _ = key `elem` selection

