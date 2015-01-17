module Yage.Resource.YageResource
  ( YageResource
  , module Data.Acquire
  , module Control.Monad.Trans.Resource
  ) where

import Data.Acquire
import Control.Monad.Trans.Resource

type YageResource = Acquire
