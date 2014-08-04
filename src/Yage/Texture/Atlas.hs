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

data AtlasSettings a = AtlasSettings
    { _atlSizePx       :: V2 Int
    -- ^ `sizePx` in pixels starting from 1 `V2 [1..w] [1..h]`
    , _atlBackground   :: a
    -- ^ `background` color-value
    , _atlPaddingPx    :: Int
    -- ^ `paddingPx` for each element added to the atlas
    } deriving ( Show, Eq, Ord )

makeLenses ''AtlasSettings


data TextureAtlas i a = TextureAtlas
    { _atlasRegions     :: AtlasTree i a
    , _regionIds        :: Set i
    , _atlasSettings    :: AtlasSettings a
    }
makeLenses ''TextureAtlas


---------------------------------------------------------------------------------------------------

emptyNode :: TextureRegion -> AtlasTree i a
emptyNode rect = Node rect Nil Nil


filledLeaf :: i -> Image a -> Int -> AtlasTree i a
filledLeaf ident img padding =
    let rect = imageRectangle img & extend +~ pure (2 * padding)
    in Filled (AtlasRegion rect padding ident img)


-- | creates an empty atlas
emptyAtlas :: AtlasSettings a
           -- ^ settings
           -> TextureAtlas i a
           -- ^ empty atlas
emptyAtlas settings =
    let root     = Node rect Nil Nil
        rect     = Rectangle 0 (settings^.atlSizePx - 1)
    in TextureAtlas root empty settings


insertImage :: (Ord i) => i -> Image a -> TextureAtlas i a -> Either (AtlasError i) (TextureAtlas i a)
insertImage ident img atlas =
    if atlas^.regionIds.contains ident
        then Left $ AlreadyContained ident
        else either (const $ Left NoSpace) Right $ insert ident img atlas

    where

    insert :: (Ord i) => i -> Image a -> TextureAtlas i a -> Either (TextureAtlas i a) (TextureAtlas i a)
    insert ident img atlas =
        let regions  = insertNode (filledLeaf ident img (atlas^.atlasSettings.atlPaddingPx)) (atlas^.atlasRegions)
            newAtlas = atlas & atlasRegions .~ regions^.chosen
                             & regionIds    %~ (contains ident .~ isRight regions)
        in either (const $ Left atlas) (const $ Right newAtlas) regions



insertNode :: AtlasTree i a -> AtlasTree i a -> Either (AtlasTree i a) (AtlasTree i a)
-- empty region, split and insert left
insertNode filled@(Filled a) tree@(Node nodeRegion Nil Nil)

    -- insert rect to big
    | not $ (a^.regionRect ) `fits` nodeRegion       = Left tree

    -- perfectly fits
    | a^.regionRect.extend == nodeRegion^.extend    =   let padding = a^.regionPadding
                                                            rect    = nodeRegion & xy1  +~ pure padding
                                                                                 & xy2  -~ pure padding
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
foldrWithFilled f = go where
    go z' Nil = z'
    go z' (Filled a)= f a z'
    go z' (Node _ l r) = go (go z' r) l



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
    in get mReg where

    get (Just region) = pixelAt (region^.regionData)
                                (x - region^.regionRect.xy1._x)
                                (y - region^.regionRect.xy1._y)
    get _             = atlas^.atlasSettings.atlBackground



splitRect :: TextureRegion -> TextureRegion -> (TextureRegion, TextureRegion)
splitRect toSplit toFit =
    let direction = if dw > dh then SplitVertical else SplitHorizontal
    in split direction where

    split SplitVertical     = ( toSplit & xy2._x -~ dw
                              , toSplit & xy1._x +~ ( toFit^.width + 1 )
                              )
    split SplitHorizontal   = ( toSplit & xy2._y -~ dh
                              , toSplit & xy1._y +~ ( toFit^.height + 1 )
                              )
    dw = toSplit^.width - toFit^.width
    dh = toSplit^.height - toFit^.height

instance Foldable.Foldable (Tree region) where
    foldr = foldrWithFilled

