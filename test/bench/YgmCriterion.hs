{-# LANGUAGE OverloadedStrings #-}
module Main where

import Yage.Prelude
import Foreign.C.Types
import Criterion.Main
import Yage.Formats.Ygm
import Yage.Resources
import Yage.Resources
import Yage.Pipeline.Deferred.GeometryPass

main :: IO ()
main = do
    -- x <- force <$> loadYGM geoVertex ( "test" </> "res" </> "head.ygm", mkSelection [] )
    -- print $ _meshId x

    defaultMain
        [ bench "ygm head " $ nfIO (loadYGM geoVertex ( "test" </> "res" </> "head.ygm", mkSelection [] ))
        , bench "ygm sponza" $ nfIO (loadYGM geoVertex ( "test" </> "res" </> "sponza.ygm", mkSelection [] ))
        ]

instance NFData CFloat where
    rnf x = x `seq` ()
