{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE UndecidableInstances            #-}
module Yage.Texture.Atlas.Builder
    ( module Yage.Texture.Atlas.Builder
    , module A
    ) where

import           Yage.Prelude
import           Yage.Lens hiding ((<.>), (.=))
import           Yage.Math

import           Yage.Image

import           Data.Aeson              ( ToJSON, FromJSON, (.=), (.:) )
import qualified Data.Aeson    as JSON
import           Text.Read

import qualified Data.Map      as Map

import           Yage.Texture.Atlas as A
import           Yage.Texture.TextureAtlas as T
import           Filesystem.Path.CurrentOS hiding ( null )


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


-- | Intermediate building structure. An atlas filled with distinct images in
-- it's tree structure.
type ImageAtlas i px = Atlas i (AtlasSettings px) (Image px)


type AtlasResult i a = ([(i, AtlasError i)], ImageAtlas i a)

data AtlasJSON i = AtlasJSON
    { jsonTextureFile :: FilePath
    , jsonRegions     :: RegionMap i
    , jsonPadding     :: Int
    } deriving ( Show, Eq, Generic )


instance Show i => ToJSON (AtlasJSON i) where
    toJSON AtlasJSON{..} = JSON.object [ "textureFile" .= fpToText jsonTextureFile, "regions" .= Map.mapKeys show jsonRegions, "padding" .= jsonPadding ]

instance ( Read i, Ord i ) => FromJSON (AtlasJSON i) where
    parseJSON (JSON.Object v) = do
        fp        <- decodeString <$> v .: "textureFile"
        regionmap <- Map.mapKeys read <$> v .: "regions"
        AtlasJSON fp regionmap <$> v .: "padding"

    parseJSON _ = mzero

---------------------------------------------------------------------------------------------------

newImageAtlas :: (Ord i, Pixel a)
         => AtlasSettings a
         -> [(i, Image a)]
         -> AtlasResult i a
newImageAtlas settings idTexs =
    let sortedTex = sortBy (descending imageByAreaCompare `on` snd) idTexs
        initRect  = Rectangle 0 ( settings^.atlSizePx & _xy -~ 1 )
    in insertImages sortedTex $ emptyAtlas settings & A.atlasRegions .~ emptyNode initRect


regionMap :: (Ord i) => ImageAtlas i a -> RegionMap i
regionMap atlas = foldrWithFilled collectFilled Map.empty $ atlas^.A.atlasRegions

    where

    collectFilled :: (Ord i) => AtlasRegion i a -> Map i TextureRegion -> Map i TextureRegion
    collectFilled region =
        let ident = region^.regionId
            rect  = region^.regionRect
        in at ident ?~ rect


---------------------------------------------------------------------------------------------------

insertImages :: (Ord i) => [(i, Image a)] -> ImageAtlas i a -> AtlasResult i a
insertImages [] atlas = ([], atlas)
insertImages ((ident, img):imgs) atlas =
    either err success $ insertImage ident img atlas

    where

    err e       = joinResults (ident, e) (insertImages imgs atlas)
    success     = insertImages imgs
    joinResults e (errs, atlas') = (e:errs, atlas')


insertImage :: (Ord i) => i -> Image a -> ImageAtlas i a -> Either (AtlasError i) (ImageAtlas i a)
insertImage ident img atlas =
    if atlas^.regionIds.contains ident
        then Left $ AlreadyContained ident
        else either (const $ Left NoSpace) Right $ insert ident img atlas

    where

    insert :: (Ord i) => i -> Image a -> ImageAtlas i a -> Either (ImageAtlas i a) (ImageAtlas i a)
    insert ident img atlas =
        let regions  = insertNode (filledLeaf ident img (atlas^.atlasData.atlPaddingPx)) (atlas^.A.atlasRegions)
            newAtlas = atlas & A.atlasRegions .~ regions^.chosen
                             & regionIds      %~ (contains ident .~ isRight regions)
        in either (const $ Left atlas) (const $ Right newAtlas) regions


getAtlasPixel :: forall i px. (Pixel px) => ImageAtlas i px -> Int -> Int -> px
getAtlasPixel atlas x y =
    let mReg :: Maybe (AtlasRegion i (Image px))
        mReg = getRegionAt atlas x y
    in get mReg where

    get :: Maybe (AtlasRegion i (Image px)) -> px
    get (Just region) = pixelAt (region^.regionData)
                                (x - region^.regionRect.xy1._x)
                                (y - region^.regionRect.xy1._y)
    get _             = atlas^.atlasData.atlBackground



atlasToImage :: (Pixel a) => ImageAtlas i a -> Image a
atlasToImage atlas =
    generateImage (getAtlasPixel atlas)
                  (atlas^.atlasData.atlSizePx._x)
                  (atlas^.atlasData.atlSizePx._y)

---------------------------------------------------------------------------------------------------

newTextureAtlas
    :: (Ord i, Pixel a)
    => AtlasSettings a
    -> [(i, Image a)]
    -> Either [(i, AtlasError i)] (TextureAtlas i (Image a))
newTextureAtlas settings imgs =
    let (err, atlas) = newImageAtlas settings imgs
    in if null err
        then Right $ buildTextureAtlas atlas
        else Left err

-- | converts the intermediate image atlas structure to a merged texture atlas
buildTextureAtlas :: (Ord i, Pixel px) => ImageAtlas i px -> TextureAtlas i (Image px)
buildTextureAtlas imageAtlas =
    TextureAtlas
        { _atlasRegions  = regionMap imageAtlas
        , _atlasImage    = atlasToImage imageAtlas
        , _atlasPadding  = imageAtlas^.atlasData.atlPaddingPx
        }


-- | TODO : bundle in archive
writeTextureAtlas :: ( Ord i, Show i, PngSavable px ) => FilePath -> TextureAtlas i (Image px) -> IO ()
writeTextureAtlas filepath atlas =
    let filepathPNG  = replaceExtension filepath "png"
        filepathJSON = replaceExtension filepath "json.yat"
    in do
        writePng (fpToString filepathPNG) $ atlas^.atlasImage
        writeRegionMapJSON filepathJSON $ AtlasJSON { jsonTextureFile = filename filepathPNG
                                                    , jsonRegions     = atlas^.T.atlasRegions
                                                    , jsonPadding     = atlas^.atlasPadding
                                                    }
    where

    writeRegionMapJSON :: ToJSON (AtlasJSON i) => FilePath -> AtlasJSON i -> IO ()
    writeRegionMapJSON filePath atlasJson = writeFile filePath (JSON.encode atlasJson)


readTextureAtlas :: FromJSON (AtlasJSON i) => FilePath -> IO ( TextureAtlas i DynamicImage )
readTextureAtlas yatFile = do
    eAtlasJson <- JSON.eitherDecode <$> readFile yatFile
    either error toTextureAtlas eAtlasJson

    where

    toTextureAtlas :: AtlasJSON i -> IO ( TextureAtlas i DynamicImage )
    toTextureAtlas AtlasJSON{..} =
        let imgFilePath = directory yatFile </> jsonTextureFile
        in do
            eImg <- readImage ( fpToString imgFilePath )
            either error return $ do
                img <- eImg
                return $ TextureAtlas
                    { _atlasRegions  = jsonRegions
                    , _atlasImage    = img
                    , _atlasPadding  = jsonPadding
                    }
