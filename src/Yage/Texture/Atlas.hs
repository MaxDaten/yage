{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
module Yage.Texture.Atlas
    ( module Yage.Texture.Atlas
    , module Rectangle
    ) where

import           Yage.Prelude
import           Yage.Lens

import           Yage.Images
import           Yage.Math


import qualified Data.Foldable as Foldable
import qualified Data.Map      as Map
import           Data.Set      hiding (foldr, null)

import           Yage.Geometry.D2.Rectangle as Rectangle

data Tree region fill =
      Node region (Tree region fill) (Tree region fill)
    | Filled fill
    | Nil

data AtlasError i =
      AlreadyContained i
    | NoSpace
    deriving (Show)

data SplitDirection = SplitHorizontal | SplitVertical

type TextureRegion = Rectangle Int
data AtlasRegion i a = AtlasRegion
    { _regionRect    :: !TextureRegion
    , _regionPadding :: !Int
    , _regionId      :: !i
    , _regionData    :: !(Image a)
    }

makeLenses ''AtlasRegion


type AtlasTree i a = Tree TextureRegion (AtlasRegion i a)
data TextureAtlas i a = TextureAtlas
    { _atlasSize    :: (Int, Int)
    , _atlasRegions :: AtlasTree i a
    , _regionIds    :: Set i
    , _background   :: a
    , _padding      :: Int
    }
makeLenses ''TextureAtlas

type RegionMap i = Map i TextureRegion

---------------------------------------------------------------------------------------------------

emptyNode :: TextureRegion -> AtlasTree i a
emptyNode rect = Node rect Nil Nil


filledLeaf :: i -> Image a -> Int -> AtlasTree i a
filledLeaf ident img padding =
    let rect = imageRectangle img & extend +~ pure (2 * padding)
    in Filled (AtlasRegion rect padding ident img)


emptyAtlas :: Int -> Int -> a -> Int -> TextureAtlas i a
emptyAtlas width height background padding =
    let root = Node rect Nil Nil
        rect = Rectangle 0 (V2 width height)
    in TextureAtlas
    { _atlasSize    = (width, height)
    , _atlasRegions = root
    , _regionIds    = empty
    , _background   = background
    , _padding      = padding
    }


insertImage :: (Ord i) => i -> Image a -> TextureAtlas i a -> Either (AtlasError i) (TextureAtlas i a)
insertImage ident img atlas =
    if atlas^.regionIds.contains ident
    then Left $ AlreadyContained ident
    else either (const $ Left NoSpace) Right $ insert ident img atlas
    where
        insert :: (Ord i) => i -> Image a -> TextureAtlas i a -> Either (TextureAtlas i a) (TextureAtlas i a)
        insert ident img atlas =
            let regions  = insertNode (filledLeaf ident img (atlas^.padding)) (atlas^.atlasRegions)
                newAtlas =  atlasRegions .~ regions^.chosen $
                            regionIds    %~ (contains ident .~ isRight regions) $ atlas
            in either (const $ Left atlas) (const $ Right newAtlas) regions


insertNode :: AtlasTree i a -> AtlasTree i a -> Either (AtlasTree i a) (AtlasTree i a)
-- empty region, split and insert left
insertNode filled@(Filled a) tree@(Node nodeRegion Nil Nil)
    
    -- insert rect to big
    | not $ (a^.regionRect) `fits` nodeRegion         = Left tree
    
    -- perfectly fits
    | (a^.regionRect.extend) == nodeRegion^.extend    = let padding = a^.regionPadding
                                                            rect    = nodeRegion & topLeft     +~ pure padding
                                                                                 & bottomRight -~ pure padding
                                                        in Right $ Filled (a & regionRect .~ rect)
    
    -- split region in one half fitting and retaining region, then insert into the half fitting one
    | otherwise =
        let (leftRegion, rightRegion) = over both emptyNode $ splitRect nodeRegion (a^.regionRect)
        in insertNode filled $ Node nodeRegion leftRegion rightRegion
insertNode _ (Filled a)             = Left $ Filled a
insertNode _ Nil                    = Left Nil
insertNode node tree@(Node _ l r)   = case insertNode node l of
                                        Right l' -> Right $ setLeft l' tree
                                        Left _   -> flip setRight tree <$> insertNode node r
-- insertNode _ _                      = error "invalid insertion"


setLeft :: Tree a b -> Tree a b -> Tree a b
setLeft node (Node a _ r) = Node a node r
setLeft _ _ = error "invalid Atlas.hs: setLeft"


setRight :: Tree a b -> Tree a b -> Tree a b
setRight node (Node a l _) = Node a l node
setRight _ _ = error "invalid Atlas.hs: setRight"


foldrWithFilled :: (a -> b -> b) -> b -> Tree c a -> b
foldrWithFilled f = go
    where
        go z' Nil = z'
        go z' (Filled a)= f a z'
        go z' (Node _ l r) = go (go z' r) l

---------------------------------------------------------------------------------------------------


type AtlasResult i a = ([(i, AtlasError i)], TextureAtlas i a)

insertImages :: (Ord i) => [(i, Image a)] -> TextureAtlas i a -> AtlasResult i a
insertImages [] atlas = ([], atlas)
insertImages ((ident, img):imgs) atlas =
    either err success $ insertImage ident img atlas
    where
        err e       = joinResults (ident, e) (insertImages imgs atlas)
        success     = insertImages imgs
        joinResults e (errs, atlas) = (e:errs, atlas)


regionMap :: (Ord i) => TextureAtlas i a -> RegionMap i
regionMap atlas = foldrWithFilled collectFilled Map.empty $ atlas^.atlasRegions
    where
        collectFilled :: (Ord i) => AtlasRegion i a -> Map i TextureRegion -> Map i TextureRegion
        collectFilled region =
                let ident = region^.regionId
                    rect  = region^.regionRect
                in at ident ?~ rect


isInNode :: AtlasTree i a -> Int -> Int -> Bool
isInNode (Node region _ _) x y  = region `containsPoint` (V2 x y)
isInNode (Filled a) x y         = (a^.regionRect) `containsPoint` (V2 x y)
isInNode _ _ _                  = False


getFilledAt :: AtlasTree i a -> Int -> Int -> Maybe (AtlasTree i a)
getFilledAt root@(Node region _l _r) x y
    | region `containsPoint` at   = getNodeIn root
    | otherwise                   = error "out of bounds"
    where
        at = V2 x y
        getNodeIn (Node _ Nil Nil)               = Nothing
        getNodeIn (Filled a)
            | (a^.regionRect) `containsPoint` at = Just $ Filled a
            | otherwise                          = Nothing
        getNodeIn Nil                            = Nothing
        getNodeIn (Node _ l r)                   = getNodeIn (if isInNode l x y then l else r)
getFilledAt _ _ _ = error "not a valid root"


getRegionAt :: TextureAtlas i a -> Int -> Int -> Maybe (AtlasRegion i a)
getRegionAt atlas x y = toRegion <$> getFilledAt (atlas^.atlasRegions) x y
    where
        toRegion (Filled a) = a
        toRegion _          = error "invalid node"


getAtlasPixel :: (Pixel a) => TextureAtlas i a -> Int -> Int -> a
getAtlasPixel atlas x y =
    let mReg = getRegionAt atlas x y
    in get mReg
    where
        get (Just region) = pixelAt (region^.regionData)
                                    (x - region^.regionRect.topLeft._x)
                                    (y - region^.regionRect.topLeft._y)
        get _             = atlas^.background


atlasToImage :: (Pixel a) => TextureAtlas i a -> Image a
atlasToImage atlas = generateImage (getAtlasPixel atlas) (atlas^.atlasSize._1) (atlas^.atlasSize._2)


splitRect :: TextureRegion -> TextureRegion -> (TextureRegion, TextureRegion)
splitRect toSplit toFit =
    let dw        = toSplit^.width - toFit^.width
        dh        = toSplit^.height - toFit^.height
        direction = if dw > dh then SplitVertical else SplitHorizontal
    in split direction
    where
        split SplitVertical     = ( toSplit & bottomRight._x -~ (toFit^.width - 1)
                                  , toSplit & topLeft._x     +~ (toFit^.width)
                                  )
        split SplitHorizontal   = ( toSplit & bottomRight._y +~ (toFit^.height - 1)
                                  , toSplit & topLeft._y     -~ toFit^.height 
                                  )


instance Foldable.Foldable (Tree region) where
    foldr = foldrWithFilled

