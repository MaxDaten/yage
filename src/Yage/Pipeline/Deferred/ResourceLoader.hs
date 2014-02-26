module Yage.Pipeline.Deferred.ResourceLoader where

import Yage.Resources
import Yage.Geometry

import Yage.Pipeline.Deferred.Spec

deferredResourceLoader :: ResourceLoader GeoVertex
deferredResourceLoader = ResourceLoader
    { objLoader = loadOBJ }
