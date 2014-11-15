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

keyStateIs :: Key -> KeyState -> KeyEvent -> Bool
keyStateIs key state (KeyEvent k _int s _mod) = key == k && s == state

isMouseMoveEvent :: MouseEvent -> Bool
isMouseMoveEvent (MouseMoveEvent _) = True
isMouseMoveEvent _ = False

clearEvents :: InputState -> InputState
clearEvents i = i & mouseEvents     .~ mempty
                  & keyboardEvents  .~ mempty
