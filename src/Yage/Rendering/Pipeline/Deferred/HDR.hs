{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, TypeOperators #-}

module Yage.Rendering.Pipeline.Deferred.HDR where

import Yage.Prelude                                         hiding ( toList, last, head )
import Yage.Lens                                            hiding ( cons )

import Data.List                                            ( last, tail )

import Yage.HDR
import Yage.Scene
import Yage.Material

import qualified Yage.Rendering.Pipeline.Deferred.LightPass             as L
import qualified Yage.Rendering.Pipeline.Deferred.BaseGPass             as G
import qualified Yage.Rendering.Pipeline.Deferred.SkyPass               as S
import qualified Yage.Rendering.Pipeline.Deferred.GlareDetectionPass    as Glare
import qualified Yage.Rendering.Pipeline.Deferred.ToneMapPass           as T
import Yage.Rendering.Pipeline.Deferred.GaussFilter


