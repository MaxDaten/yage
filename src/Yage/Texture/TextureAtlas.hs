{-# LANGUAGE TemplateHaskell #-}
module Yage.Texture.TextureAtlas
    ( module Yage.Texture.TextureAtlas
    , TextureRegion
    ) where

import Yage.Prelude
import Yage.Lens

import Yage.Texture.Atlas

type RegionMap i = Map i TextureRegion

data TextureAtlas i img = TextureAtlas
    { _atlasRegions  :: RegionMap i
    , _atlasImage    :: img
    , _atlasPadding  :: Int
    }

makeLenses ''TextureAtlas
