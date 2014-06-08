{-# LANGUAGE OverloadedStrings #-}
import Yage.Prelude
import Criterion.Main
import Yage.Formats.Obj.Parser

criterion :: IO ()
criterion = defaultMain
    [ bgroup "parseOBJFile"
        [ bench "very simple cube"   $ nfIO (parseOBJFile $ "test" </> "res" </> "cube.obj")
        --, bench ""
        ]
    ]

main :: IO ()
main = criterion