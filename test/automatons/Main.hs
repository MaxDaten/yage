{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where
import Test.Hspec (hspec, describe)
import StorableVertex

main :: IO ()
main = do
    hspec $ do
        describe "storable vertex" $ do
            vertexSpecs
