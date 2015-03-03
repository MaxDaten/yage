{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE PatternGuards          #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE Arrows                 #-}

module Yage.Rendering.Pipeline.Voxel.Base
  () where

import           Yage.Prelude hiding ((</>), cons)
import           Yage.Lens hiding (cons)
import           Yage.Math
import           Yage.Vertex hiding (Texture)
import           Yage.Formats.Ygm


import           Yage.HDR
import           Yage.Rendering.GL
import           Yage.Rendering.RenderSystem                     as RenderSystem
import           Yage.Rendering.RenderTarget
import           Yage.Rendering.Resources.GL
import           Yage.Scene
import           Yage.Viewport
import           Yage.Material hiding (over)

import           Yage.Rendering.Pipeline.Voxel.Voxelize           as Pass
import           Yage.Rendering.Pipeline.Voxel.VisualizeVoxel     as Pass
import           Yage.Rendering.Pipeline.Voxel.UnpackVoxel        as Pass
import           Yage.Rendering.Pipeline.Deferred.Common
import           Yage.Rendering.Pipeline.Deferred.Types

import           Control.Arrow
import           Quine.GL.Shader
import           Quine.StateVar
import           Quine.GL.Types
import           Quine.GL.Texture


voxelize :: (HasScene a DeferredEntity DeferredEnvironment, HasHDRCamera a, DeferredMonad m env)
  => Int -> Int -> Int -> YageResource (RenderSystem m a (Texture3D PixelRGB8))
voxelize width height depth = do
  voxelizeScene   <- voxelizePass width height depth
  unpackVoxel     <- unpackVoxelPass width height depth
  return $ proc input -> do
    mainViewport  <- currentViewport -< ()
    voxelizedScene   <- processPass voxelizeScene    -< input^.scene
    rgbVoxel         <- processPass unpackVoxel      -< voxelizedScene
    returnA -< rgbVoxel

{--
    voxelizedScene   <- processPass voxelizeScene    -< input^.scene
    rgbVoxel         <- processPass unpackVoxel      -< voxelizedScene
    voxelSceneTarget <- autoResized mkVisVoxelTarget -< mainViewport^.rectangle
    voxelScene       <- processPassWithGlobalEnv voxelVis
                         -< ( voxelSceneTarget
                            , voxelizedScene
                            , rgbVoxel
                            , eye4 & _xyz *~ 4
                            , input^.hdrCamera.camera
                            , [VisualizeSceneVoxel, VisualizePageMask]
                            )

--}
