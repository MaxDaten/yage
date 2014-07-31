{-# LANGUAGE TemplateHaskell #-}
module Yage.UI.GUI
    ( module Yage.UI.GUI
    , module Yage.Font
    ) where

import           Yage.Prelude
import           Yage.Lens
import           Yage.Math               hiding ( lerp )

import           Yage.Font
import           Yage.Camera
import           Yage.Transformation
import qualified Data.Map.Lazy           as M
import           Yage.Resources


data GUIElement =
      GUIGraphic -- currently just a place holder
    | GUIFont TextBuffer (Transformation Float)
    deriving ( Show )

data GUI = GUI
    { _guiCamera   :: Camera
    , _guiElements :: Map ByteString GUIElement
    } deriving ( Show )

makeLenses ''GUI

emptyGUI :: GUI
emptyGUI = GUI (mkCameraGui (-1, 1)) M.empty


-- | creates a camera suitable for 2D gui rendering.
--
-- for gui rendering it's a common practice to stack the elements along the z-axis
-- like layering in photoshop.
-- The layer range is defined by the near and far plane.
-- The camera is looking along the positive z-axis. To make layering more intuitive
-- the camera is moved to z=far.
-- ordering of viewable layers is in the ascending range [0..100] (floating)
mkCameraGui :: (Double, Double) -> Camera
mkCameraGui nearFar@(near, _) =
    -- fov is not needed for 2d, viewfield in 2d just defined with ortho-matrix
    mkCameraFps 90 nearFar idTransformation
        & cameraLocation._z .~ realToFrac near


instance LinearInterpolatable GUI where
    lerp alpha u v =
        u & guiCamera   .~ lerp alpha (u^.guiCamera) (v^.guiCamera)
          & guiElements .~ M.intersectionWith (lerp alpha) (u^.guiElements) (v^.guiElements)

instance LinearInterpolatable GUIElement where
    -- TODO element based lerping
    lerp _alpha _u v = v

instance HasResources vert GUI GUI where
    requestResources = return
