{-# LANGUAGE TemplateHaskell #-}
module Yage.Texture.Atlas where

import Yage.Prelude

import Data.List (zip, null, sortBy)
import Data.Set hiding (foldr, null)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable
import Data.Maybe (fromJust)
import Control.Lens
import Control.Monad (when)
import Codec.Picture


data Tree region fill = 
      Node region (Tree region fill) (Tree region fill) 
    | Filled fill
    | Nil


data AtlasError i =
      AlreadyContained i
    | NoSpace
    deriving (Show)

data SplitDirection = SplitHorizontal | SplitVertical

data Rectangle = Rectangle
    { _x0, _y0, _x1, _y1 :: !Int }
    deriving (Show)
makeLenses ''Rectangle

width rect  = rect^.x1 - rect^.x0 + 1
height rect = rect^.y1 - rect^.y0 + 1

instance Eq Rectangle where
    r1 == r2 = 
       r1^.x0 == r2^.x0 && 
       r1^.y0 == r2^.y0 &&
       r1^.x1 == r2^.x1 &&
       r1^.y1 == r2^.y1


instance Ord Rectangle where
    compare r1 r2 = 
        let w1 = r1^.to width
            h1 = r1^.to height
            w2 = r2^.to width
            h2 = r2^.to height
        in compare (w1*h1) (w2*h2)

data AtlasRegion i a = AtlasRegion
    { _regionRect    :: !Rectangle
    , _regionPadding :: !Int
    , _regionId      :: !i
    , _regionData    :: !(Image a)
    }

makeLenses ''AtlasRegion


type AtlasTree i a = Tree Rectangle (AtlasRegion i a)
data TextureAtlas i a = TextureAtlas
    { _atlasSize    :: (Int, Int)
    , _atlasRegions :: AtlasTree i a
    , _regionIds    :: Set i
    , _background   :: a
    , _padding      :: Int
    }
makeLenses ''TextureAtlas


emptyNode :: Rectangle -> AtlasTree i a
emptyNode rect = Node rect Nil Nil


filledLeaf :: i -> Image a -> Int -> AtlasTree i a
filledLeaf ident img padding =
    let rect = x1 +~ 2*padding $
               y1 +~ 2*padding $ 
               img^.to imgRectangle
    in Filled (AtlasRegion rect padding ident img)


emptyAtlas :: Int -> Int -> a -> Int -> TextureAtlas i a
emptyAtlas width height background padding = 
    let root = Node rect Nil Nil
        rect = Rectangle 0 0 width height
    in TextureAtlas
    { _atlasSize    = (width, height)
    , _atlasRegions = root
    , _regionIds    = empty
    , _background   = background
    , _padding      = padding
    }

insertImage :: (Ord i) => i -> Image a -> TextureAtlas i a -> Either (AtlasError i) (TextureAtlas i a)
insertImage ident img atlas = traceShow "==insert==" $
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
insertNode filled@(Filled ~a) tree@(Node ~nodeRegion Nil Nil)
    -- insert rect to big
    | not $ (a^.regionRect) `fits` nodeRegion             = traceShow "to big" $ Left tree
    -- perfectly fits
    | (a^.regionRect) `dimMatches` nodeRegion             = let padding = a^.regionPadding
                                                                rect    = x0 +~ padding $ y0 +~ padding $ 
                                                                          x1 -~ padding $ y1 -~ padding $ nodeRegion
                                                            in Right $ Filled (a & regionRect .~ rect)
    -- split region in one half fitting and retaining region, then insert into the half fitting one
    | otherwise = 
        let (leftRegion, rightRegion) = emptyNode <$$> (traceShow' $ splitRect (nodeRegion) (a^.regionRect))
        in traceShow "split"
            $ insertNode filled $ Node nodeRegion leftRegion rightRegion
insertNode _ (Filled a)             = Left $ Filled a
insertNode _ Nil                    = Left Nil
insertNode node tree@(Node _ l r)   = case (insertNode node l) of 
                                        Right l' -> Right $ setLeft l' tree
                                        Left _   -> flip setRight tree <$> insertNode node r
insertNode _ _                      = error "invalid insertion"


setLeft :: Tree a b -> Tree a b -> Tree a b
setLeft node (Node ~a _ ~r) = Node a node r

setRight :: Tree a b -> Tree a b -> Tree a b
setRight node (Node ~a ~l _) = Node a l node


foldrWithFilled :: (a -> b -> b) -> b -> Tree c a -> b
foldrWithFilled f z = go z
    where
        go z' Nil = z'
        go z' (Filled ~a)= f a z' 
        go z' (Node _ ~l ~r) = go (go z' r) l

---------------------------------------------------------------------------------------------------


type AtlasResult i a = ([(i, AtlasError i)], TextureAtlas i a)

insertImages :: (Ord i) => [(i, Image a)] -> TextureAtlas i a -> AtlasResult i a
insertImages [] atlas = ([], atlas) 
insertImages ((ident, img):imgs) atlas = 
    either err success $ insertImage ident img atlas
    where
        err e            = joinResults (ident, e) (insertImages imgs atlas)
        success newAtlas = insertImages imgs newAtlas
        joinResults e (errs, atlas) = (e:errs, atlas)

regionMap :: (Ord i) => TextureAtlas i a -> Map i Rectangle
regionMap atlas = foldrWithFilled collectFilled Map.empty $ atlas^.atlasRegions
    where
        collectFilled :: (Ord i) => AtlasRegion i a -> Map i Rectangle -> Map i Rectangle
        collectFilled region = 
                let ident = region^.regionId
                    rect  = region^.regionRect
                in at ident ?~ rect


isInNode :: AtlasTree i a -> Int -> Int -> Bool
isInNode (Node region _ _) x y  = inRectangle x y region
isInNode (Filled a) x y         = inRectangle x y (a^.regionRect)
isInNode _ _ _                  = False


getFilledAt :: AtlasTree i a -> Int -> Int -> Maybe (AtlasTree i a)
getFilledAt root@(Node region l r) x y 
    | inRectangle x y region    = getNodeIn root
    | otherwise                 = error "out of bounds"
    where
        getNodeIn (Node _ Nil Nil)            = Nothing
        getNodeIn (Filled a)
            | inRectangle x y (a^.regionRect) = Just $ Filled a
            | otherwise                       = Nothing
        getNodeIn Nil                         = Nothing
        getNodeIn (Node _ l r)                = getNodeIn (if isInNode l x y then l else r)
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
        get (Just region) = pixelAt (region^.regionData) (x - region^.regionRect.x0) (y - region^.regionRect.y0)
        get _             = atlas^.background


atlasToImage :: (Pixel a) => TextureAtlas i a -> Image a
atlasToImage atlas = generateImage (getAtlasPixel atlas) (atlas^.atlasSize._1) (atlas^.atlasSize._2) 


subImage :: (Pixel a) => Image a -> Rectangle -> Image a -> Image a
subImage sub region target 
    | check = generateImage (includeRegionImg sub region target) (imageWidth target) (imageHeight target)
    where
        includeRegionImg sub region target x y 
            | inRectangle x y region = pixelAt sub (x - region^.x0) (y - region^.y0) 
            | otherwise              = pixelAt target x y
        
        check
            | imageWidth sub  /= region^.to width ||
              imageHeight sub /= region^.to height 
             = error $ "sub image to region size mismatch"

            | imageWidth sub  > imageWidth target ||
              imageHeight sub > imageHeight target
             = error $ "sub image doesn't fit into target image"

            | not $ region `inBound` (target^.to imgRectangle)
             = error $ "region not in target bounds"

            | otherwise = True


fits :: Rectangle -> Rectangle -> Bool
fits rect1 rect2 = rect1^.to width <= rect2^.to width && 
                   rect1^.to height <= rect2^.to height

dimMatches :: Rectangle -> Rectangle -> Bool
dimMatches a b =
    a^.to width  == b^.to width &&
    a^.to height == b^.to height

inBound :: Rectangle -> Rectangle -> Bool
inBound inner outer = 
    inner^.x1 <= outer^.x1 && inner^.x0 >= outer^.x0 &&
    inner^.y1 <= outer^.y1 && inner^.y0 >= outer^.y0


inRectangle :: Int -> Int -> Rectangle -> Bool
inRectangle x y rect =
    x >= rect^.x0 && x <= rect^.x1 &&
    y >= rect^.y0 && y <= rect^.y1



splitRect :: Rectangle -> Rectangle -> (Rectangle, Rectangle)
splitRect toSplit toFit = 
    let dw        = toSplit^.to width - toFit^.to width
        dh        = toSplit^.to height - toFit^.to height
        direction = if dw > dh then SplitVertical else SplitHorizontal
    in split direction
    where     
        split SplitVertical     = ( Rectangle (toSplit^.x0) (toSplit^.y0) (toSplit^.x0 + toFit^.to width - 1) (toSplit^.y1)
                                  , Rectangle (toSplit^.x0 + toFit^.to width) (toSplit^.y0) (toSplit^.x1) (toSplit^.y1))
        split SplitHorizontal   = ( Rectangle (toSplit^.x0) (toSplit^.y0) (toSplit^.x1) (toSplit^.y0 + toFit^.to height - 1) 
                                  , Rectangle (toSplit^.x0) (toSplit^.y0 + toFit^.to height) (toSplit^.x1) (toSplit^.y1))  


imgRectangle :: Image a -> Rectangle
imgRectangle img = Rectangle 0 0 (imageWidth img - 1) (imageHeight img - 1)


instance Foldable.Foldable (Tree region) where
    foldr = foldrWithFilled
