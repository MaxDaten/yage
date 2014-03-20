{-# LANGUAGE FlexibleContexts #-}
module Yage.Wire.Types where

import           Yage.Prelude
---------------------------------------------------------------------------------------------------
import           Control.Wire          (Wire, Timed, Session)
---------------------------------------------------------------------------------------------------
import           Yage.UI
---------------------------------------------------------------------------------------------------


type YageTimedInputState t = Timed t InputState
type YageSession t = Session IO (InputState -> YageTimedInputState t)
type YageWire t = Wire (YageTimedInputState t) ({--error--}) IO
