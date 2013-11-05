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

width rect = rect^.x1 - rect^.x0
height rect = rect^.y1 - rect^.y0

instance Eq Rectangle where
    r1 == r2 = 
       r1^.x0 == r2^.x0 && 
       r1^.y0 == r2^.y0 &&
       r1^.x1 == r2^.x1 &&
       r1^.y1 == r2^.y1


instance Ord Rectangle where
    compare r1 r2 = 
        let w1 = r1^.x1 - r1^.x0
            h1 = r1^.y1 - r1^.y0
            w2 = r2^.x1 - r2^.x0
            h2 = r2^.y1 - r2^.y0
        in compare (w1*h1) (w2*h2)

data AtlasRegion i a = AtlasRegion
    { _regionRect  :: !Rectangle
    , _regionId    :: !i
    , _regionData  :: !(Image a)
    }
makeLenses ''AtlasRegion


type AtlasTree i a = Tree Rectangle (AtlasRegion i a)
data TextureAtlas i a = TextureAtlas
    { _atlasSize    :: (Int, Int)
    , _atlasRegions :: AtlasTree i a
    , _regionIds    :: Set i
    , _background   :: a
    }
makeLenses ''TextureAtlas

emptyNode :: Rectangle -> AtlasTree i a
emptyNode rect = Node rect Nil Nil

filledLeaf :: i -> Image a -> AtlasTree i a
filledLeaf ident img =
    let rect = Rectangle 0 0 (imageWidth img) (imageHeight img)
    in Filled (AtlasRegion rect ident img)


emptyAtlas :: Int -> Int -> a -> TextureAtlas i a
emptyAtlas width height background' = 
    let root = Node rect Nil Nil
        rect = Rectangle 0 0 width height
    in TextureAtlas
    { _atlasSize    = (width, height)
    , _atlasRegions = root
    , _regionIds    = empty
    , _background   = background'
    }

insertImage :: (Ord i) => i -> Image a -> TextureAtlas i a -> Either (AtlasError i) (TextureAtlas i a)
insertImage ident img atlas = traceShow "==insert==" $
    if atlas^.regionIds.contains ident 
    then Left $ AlreadyContained ident
    else either (const $ Left NoSpace) Right $ insert ident img atlas
    where
        insert :: (Ord i) => i -> Image a -> TextureAtlas i a -> Either (TextureAtlas i a) (TextureAtlas i a)
        insert ident img atlas =
            let regions  = insertNode (filledLeaf ident img) (atlas^.atlasRegions)
                newAtlas =  atlasRegions .~ regions^.chosen $
                            regionIds    %~ (contains ident .~ isRight regions) $ atlas 
            in either (const $ Left atlas) (const $ Right newAtlas) regions


insertNode :: AtlasTree i a -> AtlasTree i a -> Either (AtlasTree i a) (AtlasTree i a)
-- empty region, split and insert left
insertNode filled@(Filled ~a) tree@(Node ~nodeRegion Nil Nil)
    -- insert rect to big
    | not $ (a^.regionRect) `fits` nodeRegion             = traceShow "to big" $ Left tree
    -- perfectly fits
    | (a^.regionRect) `dimMatches` nodeRegion             = traceShow "fits" $ Right $ Filled (a & regionRect .~ nodeRegion)
    -- split region in one half fitting and retaining region, then insert into the half fitting one
    | otherwise = 
        let (leftRegion, rightRegion) = emptyNode <$$> (traceShow' $ splitRect (nodeRegion) (a^.regionRect))
        in traceShow "split"
            $ insertNode filled $ Node nodeRegion leftRegion rightRegion
insertNode _ (Filled a)             = Left $ Filled a
insertNode _ Nil                    = Left Nil
insertNode node tree@(Node _ l r)   = 
    case (insertNode node l) of 
        Right l' -> Right $ setLeft l' tree
        Left _   -> flip setRight tree <$> insertNode node r
insertNode _ _                      = error "invalid insertion"

{--
insertLeft, insertRight :: AtlasTree i a -> AtlasTree i a -> Either (AtlasTree i a) (AtlasTree i a)
insertLeft node tree@(Node _ ~l _)  = traceShow "left" $ flip setLeft tree $ (insertNode node l)
insertRight node tree@(Node _ _ ~r) = traceShow "right" $ flip setRight tree $ (insertNode node r)
--}

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
        -- err :: (Ord i) => AtlasError i -> ([(i, AtlasError i)], TextureAtlas i a)
        err e = joinResults (ident, e) (insertImages imgs atlas)
        -- success :: (Ord i) => TextureAtlas i a -> ([(i, AtlasError i)], TextureAtlas i a)
        success newAtlas = insertImages imgs newAtlas
        joinResults :: (Ord i) => (i, AtlasError i) -> AtlasResult i a -> AtlasResult i a
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
    inner^.x0 < outer^.x1 && inner^.y0 < outer^.y1 &&
    inner^.x1 < outer^.x1 && inner^.y1 < outer^.y1


inRectangle :: Int -> Int -> Rectangle -> Bool
inRectangle x y rect =
    x >= rect^.x0 && x < rect^.x1 &&
    y >= rect^.y0 && y < rect^.y1



splitRect :: Rectangle -> Rectangle -> (Rectangle, Rectangle)
splitRect toSplit toFit = 
    let dw        = toSplit^.to width - toFit^.to width
        dh        = toSplit^.to height - toFit^.to height
        direction = if dw > dh then SplitVertical else SplitHorizontal
    in split direction
    where     
        split SplitVertical     = ( Rectangle (toSplit^.x0) (toSplit^.y0) (toSplit^.x0 + toFit^.to width) (toSplit^.y1)
                                  , Rectangle (toSplit^.x0 + toFit^.to width) (toSplit^.y0) (toSplit^.x1) (toSplit^.y1))
        split SplitHorizontal   = ( Rectangle (toSplit^.x0) (toSplit^.y0) (toSplit^.x1) (toSplit^.y0 + toFit^.to height) 
                                  , Rectangle (toSplit^.x0) (toSplit^.y0 + toFit^.to height) (toSplit^.x1) (toSplit^.y1))  


imgRectangle :: Image a -> Rectangle
imgRectangle img = Rectangle 0 0 (imageWidth img) (imageHeight img)

{--
main :: IO ()
main =
    let target = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 512 512
        sub    = generateImage (\x y -> PixelRGB8 (fromIntegral y) (fromIntegral x) 128) 128 128
        region = Rectangle 50 50 (50+128) (50+128)
        subbed = subImage sub region target
    in do
        writePng "target.png" target
        writePng "sub.png" sub
        writePng "subbed.png" subbed
--}


instance Foldable.Foldable (Tree region) where
    foldr = foldrWithFilled

piz = flip zip

(<$$>) :: (a -> b) -> (a, a) -> (b, b)
f <$$> (x,y) = (f x, f y) 

main :: IO ()
main =
    let --target = generateImage (const . const $ PixelRGB8 0 0 0) 512 512
        bgrnd  = PixelRGB8 0 0 0
        texs   = (sortBy imageByAreaCompare $
                         [ generateImage (const . const $ PixelRGB8 255 0 0) 20 20
                         , generateImage (const . const $ PixelRGB8 0 255 0) 60 128
                         , generateImage (const . const $ PixelRGB8 0 0 255) 70 44
                         , generateImage (const . const $ PixelRGB8 255 255 0) 128 128
                         , generateImage (const . const $ PixelRGB8 255 255 0) 52 52
                         , generateImage (const . const $ PixelRGB8 255 255 255) 256 256
                         , generateImage (const . const $ PixelRGB8 0 255 255) 64 64
                         , generateImage (const . const $ PixelRGB8 255 0 255) 64 64
                         , generateImage (const . const $ PixelRGB8 255 124 0) 128 64
                         ]) `piz` ([0..] :: [Int])
        (errs, atlas)  = insertImages texs (emptyAtlas 512 512 bgrnd)
    in do
        print $ show (errs)
        print $ show (regionMap atlas)
        when (null errs) $ writePng "atlas.png" $ atlasToImage atlas

imageByAreaCompare :: Image a -> Image a -> Ordering
imageByAreaCompare a b =
    let rA = imgRectangle a
        rB = imgRectangle b
    in compare rB rA -- flipped to sort descending
