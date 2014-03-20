{-# LANGUAGE DeriveDataTypeable         #-}
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

data MovementKeys = MovementKeys
    { _moveLeft      :: Key
    , _moveRight     :: Key
    , _moveForward   :: Key
    , _moveBackward  :: Key
    } 
    deriving ( Show, Typeable )


data MouseButtonEvent = MouseButtonEvent !MouseButton !MouseButtonState !ModifierKeys
    deriving (Show, Typeable)

data MouseState = MouseState
    { _mousePosition      :: !(V2 Double) -- | screen coords relative to upper left corner
    , _mouseScroll        :: !(V2 Double)
    , _mouseButtonEvents  :: !([MouseButtonEvent])
    }
    deriving (Show, Typeable)

makeLenses ''MouseState

---------------------------------------------------------------------------------------------------

data KeyEvent = KeyEvent !Key !Int !KeyState !ModifierKeys
    deriving (Show, Typeable)

data KeyboardState = KeyboardState
    { _keyEvents :: !([KeyEvent])
    , _keysDown  :: !(Set Key)
    } 
    deriving ( Show, Typeable )


makeLenses ''KeyboardState
---------------------------------------------------------------------------------------------------

type Axis = Double
data JoystickState = JoystickState
    { _joyButtons :: !([JoystickButtonState]) -- TODO
    , _joyAxes    :: !([Axis])
    }
    deriving (Show, Typeable)

makeLenses ''JoystickState

---------------------------------------------------------------------------------------------------

data InputState = InputState
    { _keyboard :: !KeyboardState        -- | current pressed keys
    , _mouse    :: !MouseState           -- | current pressed buttons and mouse position
    , _joystick :: !(Maybe JoystickState)  -- | current pressed buttons and axes
    }
    deriving (Show, Typeable)

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
    mempty = KeyboardState mempty mempty
    mappend (KeyboardState a a') (KeyboardState b b') = KeyboardState (a <> b) (a' <> b')

instance Semigroup KeyboardState


