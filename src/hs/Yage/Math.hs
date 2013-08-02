module Yage.Math
    (fromTransformation) where

import Linear (V3(..), V4(..), M33, M44)

fromTransformation :: M44 a -> M33 a
fromTransformation
    (V4 (V4 a b c _)
        (V4 d e f _)
        (V4 g h i _)
        (V4 j k l _)) = V3 (V3 a b c)
                           (V3 d e f)
                           (V3 g h i)