{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Yage.UI.Types
    ( module Yage.UI.Types
    , module Core
    ) where

import           Yage.Prelude
import           Yage.Lens
---------------------------------------------------------------------------------------------------
import           Yage.Core.Application as Core (Key(..), KeyState(..), MouseButton(..), MouseButtonState(..), ModifierKeys(..), JoystickButtonState(..))

import           Linear
import           Linear.Instances ()
---------------------------------------------------------------------------------------------------

data MouseEvent =
      MouseButtonEvent !MouseButton !MouseButtonState !ModifierKeys
    | MouseMoveEvent !(V2 Double)
    -- ^ screen coords relative to upper left corner
    | MouseScrollEvent !(V2 Double)
    deriving ( Show, Typeable, Generic )

---------------------------------------------------------------------------------------------------

data KeyEvent = KeyEvent !Key !Int !KeyState !ModifierKeys
    deriving ( Show, Typeable, Generic )

---------------------------------------------------------------------------------------------------

type Axis = Double
data JoystickState = JoystickState
    { _joyButtons :: !([JoystickButtonState]) -- TODO
    , _joyAxes    :: !([Axis])
    }
    deriving ( Show, Typeable, Generic )

makeLenses ''JoystickState

---------------------------------------------------------------------------------------------------

data InputState = InputState
    { _keyboardEvents :: ![KeyEvent]             -- | key pressed, released, repeated events
    , _mouseEvents    :: ![MouseEvent]           -- | pressed buttons, movement and scrolling
    , _joystick       :: !(Maybe JoystickState)  -- | current pressed buttons and axes
    }
    deriving ( Show, Typeable, Generic )

makeLenses ''InputState

initialInputState :: InputState
initialInputState = InputState
    { _keyboardEvents  = mempty
    , _mouseEvents     = mempty
    , _joystick  = Nothing --- JoystickState empty []
    }

---------------------------------------------------------------------------------------------------

instance Monoid InputState where
    mempty = initialInputState
    mappend a b =
        InputState
            { _keyboardEvents = a^.keyboardEvents <> b^.keyboardEvents
            , _mouseEvents    = a^.mouseEvents    <> b^.mouseEvents
            , _joystick = a^.joystick <> b^.joystick
            }

instance Semigroup InputState


instance Monoid JoystickState where
    mempty = JoystickState mempty mempty
    mappend a b =
        JoystickState
            { _joyButtons = a^.joyButtons <> b^.joyButtons
            , _joyAxes    = a^.joyAxes    <> b^.joyAxes
            }

instance Semigroup JoystickState


instance NFData JoystickState       where rnf = genericRnf
instance NFData JoystickButtonState where rnf = genericRnf
instance NFData MouseButton         where rnf = genericRnf
instance NFData Key                 where rnf = genericRnf
instance NFData MouseEvent          where rnf = genericRnf
instance NFData MouseButtonState    where rnf = genericRnf
instance NFData KeyEvent            where rnf = genericRnf
instance NFData ModifierKeys        where rnf = genericRnf
instance NFData KeyState            where rnf = genericRnf
instance NFData InputState          where rnf = genericRnf
