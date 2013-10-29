{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Yage.Events where

import             Yage.Prelude

import             Data.Set
import             Data.List                      (any)
import             Control.Lens

import             Yage.Core.Application


keyPressed :: Key -> Set Event -> Bool
keyPressed key' es = not.null $ filter p es
    where p (EventKey _ e)    = e^.key == key' && any (e^.keyState ==) [KeyState'Pressed, KeyState'Repeating]
          p _                 = False