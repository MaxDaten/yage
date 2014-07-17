module Yage.Texture.Atlas.Builder where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Images


import qualified Data.Map      as Map

import Yage.Texture.Atlas


---------------------------------------------------------------------------------------------------


type AtlasResult i a = ([(i, AtlasError i)], TextureAtlas i a)
type RegionMap i = Map i TextureRegion




insertImages :: (Ord i) => [(i, Image a)] -> TextureAtlas i a -> AtlasResult i a
insertImages [] atlas = ([], atlas)
insertImages ((ident, img):imgs) atlas =
    either err success $ insertImage ident img atlas
    
    where

    err e       = joinResults (ident, e) (insertImages imgs atlas)
    success     = insertImages imgs
    joinResults e (errs, atlas') = (e:errs, atlas')


regionMap :: (Ord i) => TextureAtlas i a -> RegionMap i
regionMap atlas = foldrWithFilled collectFilled Map.empty $ atlas^.atlasRegions
    
    where
    
    collectFilled :: (Ord i) => AtlasRegion i a -> Map i TextureRegion -> Map i TextureRegion
    collectFilled region =
        let ident = region^.regionId
            rect  = region^.regionRect
        in at ident ?~ rect


atlasToImage :: (Pixel a) => TextureAtlas i a -> Image a
atlasToImage atlas = 
    generateImage (getAtlasPixel atlas) 
                  (atlas^.atlasSettings.sizePx._x)
                  (atlas^.atlasSettings.sizePx._y)


newAtlas :: (Ord i, Pixel a)
         => AtlasSettings a 
         -> [(i, Image a)] 
         -> AtlasResult i a
newAtlas settings idTexs =
    let sortedTex = sortBy (descending imageByAreaCompare `on` snd) idTexs
    in insertImages sortedTex $ emptyAtlas settings
    

writeNewAtlasPNG :: (Ord i, PngSavable a, Pixel a, Show i)
                 => AtlasSettings a 
                 -> FilePath 
                 -> [(i, Image a)] 
                 -> IO ()
writeNewAtlasPNG settings filepath idTexs =
    case newAtlas settings idTexs of
        ([], atlas) -> writePng (fpToString filepath) $ atlasToImage atlas
        (err, _)    -> error $ show err
