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


--keyPressed :: Key -> Set Event -> Bool
--keyPressed key' es = not.null $ filter p es
--    where p (EventKey _ e)    = e^.key == key' && elem (e^.keyState) [KeyState'Pressed, KeyState'Repeating]
--          p _                 = False

isPressed :: InputState -> (KeyEvent -> Bool) -> Maybe KeyEvent
isPressed = flip find . _keyEvents . _keyboard

keyIs :: Key -> KeyState -> (InputState -> Bool)
keyIs key state = isJust . flip isPressed (\(KeyEvent k _ s _) -> key == k && s == state)


clearEvents :: InputState -> InputState
clearEvents i = i & mouse.mouseButtonEvents .~ mempty
                  & keyboard.keyEvents      .~ mempty