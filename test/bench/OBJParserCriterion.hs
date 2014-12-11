{-# LANGUAGE OverloadedStrings #-}
import Yage.Prelude
import Criterion.Main
import qualified Yage.Formats.Obj.Parser as A

criterion :: IO ()
criterion = defaultMain
    -- it's not a bench parsec vs. attoparsec, it's just a bench old implementation, new implementation
    [ bgroup "teapot"
        [ bench "attoparsec" $ nfIO (A.parseOBJFile $ "test" </> "res" </> "teapot.obj")
        ]
    , bgroup "head"
        [ bench "attoparsec" $ nfIO (A.parseOBJFile $ "test" </> "res" </> "head.obj")
        ]
    , bgroup "cube with groups"
        [ bench "attoparsec" $ nfIO (A.parseOBJFile $ "test" </> "res" </> "cube_groups.obj")
        ]
    ]

main :: IO ()
main = criterion
