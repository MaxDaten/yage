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
---------------------------------------------------------------------------------------------------

data MouseButtonEvent = MouseButtonEvent !MouseButton !MouseButtonState !ModifierKeys
    deriving ( Show, Typeable, Generic )

data MouseState = MouseState
    { _mousePosition      :: !(V2 Double) -- | screen coords relative to upper left corner
    , _mouseScroll        :: !(V2 Double)
    , _mouseButtonEvents  :: !([MouseButtonEvent])
    }
    deriving ( Show, Typeable, Generic )

makeLenses ''MouseState

---------------------------------------------------------------------------------------------------

data KeyEvent = KeyEvent !Key !Int !KeyState !ModifierKeys
    deriving ( Show, Typeable, Generic )

data KeyboardState = KeyboardState
    { _keyEvents :: !([KeyEvent])
    -- ^ the stream key events in order of occurrence, newest last
    }
    deriving ( Show, Typeable, Generic )


makeLenses ''KeyboardState
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
    { _keyboard :: !KeyboardState        -- | current pressed keys
    , _mouse    :: !MouseState           -- | current pressed buttons and mouse position
    , _joystick :: !(Maybe JoystickState)  -- | current pressed buttons and axes
    }
    deriving ( Show, Typeable, Generic )

makeLenses ''InputState

initialInputState :: InputState
initialInputState = InputState
    { _keyboard  = mempty
    , _mouse     = mempty
    , _joystick  = Nothing --- JoystickState empty []
    }

---------------------------------------------------------------------------------------------------

instance Monoid InputState where
    mempty = initialInputState
    mappend a b =
        InputState
            { _keyboard = a^.keyboard <> b^.keyboard
            , _mouse    = a^.mouse    <> b^.mouse
            , _joystick = a^.joystick <> b^.joystick
            }

instance Semigroup InputState


instance Monoid MouseState where
    mempty = MouseState 0 0 mempty
    mappend a b =
        MouseState
            (a^.mousePosition      +  b^.mousePosition)
            (a^.mouseScroll        +  b^.mouseScroll)
            (a^.mouseButtonEvents  <> b^.mouseButtonEvents)

instance Semigroup MouseState

instance Monoid JoystickState where
    mempty = JoystickState mempty mempty
    mappend a b =
        JoystickState
            { _joyButtons = a^.joyButtons <> b^.joyButtons
            , _joyAxes    = a^.joyAxes    <> b^.joyAxes
            }

instance Semigroup JoystickState


instance Monoid KeyboardState where
    mempty = KeyboardState mempty
    mappend (KeyboardState a) (KeyboardState b) = KeyboardState (a <> b)

instance Semigroup KeyboardState

deriving instance Generic JoystickButtonState
deriving instance Generic MouseButtonState
deriving instance Generic KeyState
deriving instance Generic ModifierKeys
deriving instance Generic MouseButton
deriving instance Generic Key
instance NFData JoystickState       where rnf = genericRnf
instance NFData JoystickButtonState where rnf = genericRnf
instance NFData MouseState          where rnf = genericRnf
instance NFData MouseButton         where rnf = genericRnf
instance NFData Key                 where rnf = genericRnf
instance NFData MouseButtonEvent    where rnf = genericRnf
instance NFData MouseButtonState    where rnf = genericRnf
instance NFData KeyboardState       where rnf = genericRnf
instance NFData KeyEvent            where rnf = genericRnf
instance NFData ModifierKeys        where rnf = genericRnf
instance NFData KeyState            where rnf = genericRnf
instance NFData InputState          where rnf = genericRnf
