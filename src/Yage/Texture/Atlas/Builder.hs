{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Texture.Atlas.Builder
    ( module Yage.Texture.Atlas.Builder
    , module Yage.Texture.Atlas
    ) where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Images


import qualified Data.Map      as Map

import Yage.Texture.Atlas


---------------------------------------------------------------------------------------------------

data AtlasSettings a = AtlasSettings
    { _atlSizePx       :: V2 Int
    -- ^ `sizePx` in pixels starting from 1 `V2 [1..w] [1..h]`
    , _atlBackground   :: a
    -- ^ `background` color-value
    , _atlPaddingPx    :: Int
    -- ^ `paddingPx` for each element added to the atlas
    } deriving ( Show, Eq, Ord )

makeLenses ''AtlasSettings

type TextureAtlas i px = Atlas i (AtlasSettings px) (Image px)


type AtlasResult i a = ([(i, AtlasError i)], TextureAtlas i a)
type RegionMap i = Map i TextureRegion


---------------------------------------------------------------------------------------------------

newAtlas :: (Ord i, Pixel a)
         => AtlasSettings a
         -> [(i, Image a)]
         -> AtlasResult i a
newAtlas settings idTexs =
    let sortedTex = sortBy (descending imageByAreaCompare `on` snd) idTexs
        initRect  = Rectangle 0 ( settings^.atlSizePx & _xy -~ 1 )
    in insertImages sortedTex $ emptyAtlas settings & atlasRegions .~ emptyNode initRect


regionMap :: (Ord i) => TextureAtlas i a -> RegionMap i
regionMap atlas = foldrWithFilled collectFilled Map.empty $ atlas^.atlasRegions

    where

    collectFilled :: (Ord i) => AtlasRegion i a -> Map i TextureRegion -> Map i TextureRegion
    collectFilled region =
        let ident = region^.regionId
            rect  = region^.regionRect
        in at ident ?~ rect

---------------------------------------------------------------------------------------------------

insertImages :: (Ord i) => [(i, Image a)] -> TextureAtlas i a -> AtlasResult i a
insertImages [] atlas = ([], atlas)
insertImages ((ident, img):imgs) atlas =
    either err success $ insertImage ident img atlas

    where

    err e       = joinResults (ident, e) (insertImages imgs atlas)
    success     = insertImages imgs
    joinResults e (errs, atlas') = (e:errs, atlas')


insertImage :: (Ord i) => i -> Image a -> TextureAtlas i a -> Either (AtlasError i) (TextureAtlas i a)
insertImage ident img atlas =
    if atlas^.regionIds.contains ident
        then Left $ AlreadyContained ident
        else either (const $ Left NoSpace) Right $ insert ident img atlas

    where

    insert :: (Ord i) => i -> Image a -> TextureAtlas i a -> Either (TextureAtlas i a) (TextureAtlas i a)
    insert ident img atlas =
        let regions  = insertNode (filledLeaf ident img (atlas^.atlasData.atlPaddingPx)) (atlas^.atlasRegions)
            newAtlas = atlas & atlasRegions .~ regions^.chosen
                             & regionIds    %~ (contains ident .~ isRight regions)
        in either (const $ Left atlas) (const $ Right newAtlas) regions


getAtlasPixel :: forall i px. (Pixel px) => TextureAtlas i px -> Int -> Int -> px
getAtlasPixel atlas x y =
    let mReg :: Maybe (AtlasRegion i (Image px))
        mReg = getRegionAt atlas x y
    in get mReg where

    get :: Maybe (AtlasRegion i (Image px)) -> px
    get (Just region) = pixelAt (region^.regionData)
                                (x - region^.regionRect.xy1._x)
                                (y - region^.regionRect.xy1._y)
    get _             = atlas^.atlasData.atlBackground



atlasToImage :: (Pixel a) => TextureAtlas i a -> Image a
atlasToImage atlas =
    generateImage (getAtlasPixel atlas)
                  (atlas^.atlasData.atlSizePx._x)
                  (atlas^.atlasData.atlSizePx._y)


writeNewAtlasPNG :: (Ord i, PngSavable a, Pixel a, Show i)
                 => AtlasSettings a
                 -> FilePath
                 -> [(i, Image a)]
                 -> IO ()
writeNewAtlasPNG settings filepath idTexs =
    case newAtlas settings idTexs of
        ([], atlas) -> writePng (fpToString filepath) $ atlasToImage atlas
        (err, _)    -> error $ show err
