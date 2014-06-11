{-# LANGUAGE OverloadedStrings #-}
import Yage.Prelude
import Criterion.Main
import qualified Yage.Formats.Obj.Parser as A 
import qualified OBJParsecParser as P

criterion :: IO ()
criterion = defaultMain
    -- it's not a bench parsec vs. attoparsec, it's just a bench old implementation, new implementation
    [ bgroup "teapot"
        [ bench "attoparsec" $ nfIO (A.parseOBJFile $ "test" </> "res" </> "teapot.obj")
        , bench "parsec" $ nfIO (P.parseOBJFile $ "test" </> "res" </> "teapot.obj") 
        ]
    , bgroup "head"
        [ bench "attoparsec" $ nfIO (A.parseOBJFile $ "test" </> "res" </> "head.obj")
        -- , bench "parsec" $ nfIO (P.parseOBJFile $ "test" </> "res" </> "head.obj") -- to slow ;)
        ]
    ]

main :: IO ()
main = criterion