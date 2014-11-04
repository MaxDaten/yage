{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Yage.UI
    ( module Yage.UI
    , module Yage.UI.Types
    ) where

import             Yage.Prelude
import             Yage.Lens
import             Yage.Core.Application.Event
import             Yage.UI.Types


clearEvents :: InputState -> InputState
clearEvents i = i & mouse.mouseButtonEvents .~ mempty
                  & keyboard.keyEvents      .~ mempty
