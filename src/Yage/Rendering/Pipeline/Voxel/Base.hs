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
  ( voxelizePass
  , module V
  ) where

import           Yage.Prelude hiding ((</>), cons)
import           Yage.Lens hiding (cons)


import           Yage.HDR
import           Yage.Rendering.RenderSystem                     as RenderSystem
import           Yage.Scene

import qualified Yage.Rendering.Pipeline.Voxel.Voxelize           as V
import           Yage.Rendering.Pipeline.Voxel.VisualizeVoxel     as V
import           Yage.Rendering.Pipeline.Voxel.UnpackVoxel        as V
import           Yage.Rendering.Pipeline.Deferred.Types


voxelizePass :: (HasScene a DeferredEntity DeferredEnvironment, HasHDRCamera a, DeferredMonad m env)
  => Int -> Int -> Int -> YageResource (RenderSystem m a VoxelScene)
voxelizePass width height depth = do
  voxelizeScene   <- V.voxelizePass width height depth
  unpackVoxel     <- unpackVoxelPass width height depth
  return $ lmap (view scene) $ processPass unpackVoxel . processPass voxelizeScene

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
