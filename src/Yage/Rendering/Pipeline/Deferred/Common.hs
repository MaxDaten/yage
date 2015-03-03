{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TemplateHaskell  #-}

module Yage.Rendering.Pipeline.Deferred.Common
  ( embeddedShaders
  , includePaths
  , currentViewport
  ) where

import Yage.Prelude hiding (FilePath)
import Yage.Lens
import Yage.Viewport
import Yage.Rendering.RenderSystem
import System.FilePath
import qualified System.FilePath.Windows as W (pathSeparator)
import qualified System.FilePath.Posix   as P (pathSeparator)
import qualified Filesystem.Path.CurrentOS as F
import Data.FileEmbed

-- | Stored outside of 'Yage.Rendering.Pipeline.Deferred' in a seperated module
-- as a try to speed up the recompile times. A seperate module changes less often
-- than a core module
embeddedShaders :: [(FilePath,ByteString)]
embeddedShaders = over (mapped._1.mapped) toPosixStyle ($(embedDir "res/glsl")) where
  toPosixStyle c | c == W.pathSeparator = P.pathSeparator
                 | otherwise = c

includePaths :: [F.FilePath]
includePaths = ["/res/glsl"]

-- | Extracs the current viewport from the current system environment
currentViewport :: (MonadReader v m, HasViewport v Int) => RenderSystem m b (Viewport Int)
currentViewport = mkStaticRenderPass $ const (view viewport)
