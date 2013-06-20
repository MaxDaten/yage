{-# LANGUAGE Arrows #-}

module Yage.Wire where

import Prelude hiding (id, (.))
import Control.Wire
import Text.Printf



countFrame :: (Monad m) => Wire e m a Int
countFrame = countFrom 0 <<< 1


-- maybe done with avgFps
avgFrameTime :: (Monad m) => Int -> Wire e m a Time
avgFrameTime n = avg n . dtime


timeString :: (Monad m) => Wire e m a String
timeString = fmap (printf "%8.2f") time