{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE PatternSynonyms #-}
module Yage.Internal.Debug
  ( installGLDebugHook
  ) where

import Yage.Prelude
import Yage.Lens
import Data.Text.Lazy.Lens

import Foreign.C.String
import Foreign.Ptr
import Graphics.GL.Core33
import Graphics.GL.Ext.KHR.Debug
import Graphics.GL.Types

import Yage.Core.Application

installGLDebugHook :: Logger -> Application AnyException ()
installGLDebugHook logger
  | gl_KHR_debug = do
    cb <- liftIO $ mkGLDEBUGPROC (glCallback logger)
    glDebugMessageCallback cb nullPtr
    glEnable GL_DEBUG_OUTPUT_SYNCHRONOUS
  | otherwise = io $ logL logger WARNING "GL_KHR_debug not supported: could not install debug hook!"


glCallback :: Logger -> GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> Ptr () -> IO ()
glCallback logger source t ident severity _ message _ = do
  message' <- peekCString message
  logL logger priority $ (format "[{}] {} ({}): {}" (Shown t', Shown source', Shown ident, Shown message'))^.unpacked
 where
  source' = case source of
    GL_DEBUG_SOURCE_API               -> "API"
    GL_DEBUG_SOURCE_WINDOW_SYSTEM     -> "Window System"
    GL_DEBUG_SOURCE_SHADER_COMPILER   -> "Shader Compiler"
    GL_DEBUG_SOURCE_THIRD_PARTY       -> "Third Party"
    GL_DEBUG_SOURCE_APPLICATION       -> "Application"
    GL_DEBUG_SOURCE_OTHER             -> "Other"
    _ -> "Unknown"

  t' = case t of
    GL_DEBUG_TYPE_ERROR               -> "Error"
    GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR -> "Deprecated Behaviour"
    GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR  -> "Undefined Behaviour"
    GL_DEBUG_TYPE_PORTABILITY         -> "Portability"
    GL_DEBUG_TYPE_PERFORMANCE         -> "Performance"
    GL_DEBUG_TYPE_OTHER               -> "Other"
    GL_DEBUG_TYPE_MARKER              -> "Marker"
    _ -> "Unknown"

  priority = case severity of
    GL_DEBUG_SEVERITY_HIGH            -> CRITICAL
    GL_DEBUG_SEVERITY_MEDIUM          -> ALERT
    GL_DEBUG_SEVERITY_LOW             -> WARNING
    GL_DEBUG_SEVERITY_NOTIFICATION    -> NOTICE
    _ -> INFO
