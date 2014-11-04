module Yage.Wire.Event where

import Data.Maybe
import Control.Wire.Unsafe.Event

maybeToEvent :: Maybe a -> Event a
maybeToEvent Nothing = NoEvent
maybeToEvent (Just e) = Event e
