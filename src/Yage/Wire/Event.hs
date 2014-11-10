module Yage.Wire.Event where

import Yage.Prelude
import Control.Wire
import Control.Wire.Unsafe.Event

maybeToEvent :: Maybe a -> Event a
maybeToEvent Nothing = NoEvent
maybeToEvent (Just e) = Event e


-- | on 'Event' picks first element from list, keeping the rest for
-- the next event until list is exhausted.
popOnEvent :: [b] -> Wire s e m (Event a) (Event b)
popOnEvent []     = never
popOnEvent (x:xs) = mkSFN $ event (NoEvent, popOnEvent (x:xs)) (const (Event x, popOnEvent xs))
