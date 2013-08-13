{-# LANGUAGE ExistentialQuantification, Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, StandaloneDeriving, TypeFamilies #-}
module Yage.Rendering.Shader where

import             Yage.Import
import             Control.Monad                 (liftM)
import             Control.Monad.IO.Class
import             Control.Monad.Trans

import             Graphics.GLUtil               (ShaderProgram, asUniform, getUniform)
import             Graphics.GLUtil.Linear        (AsUniform)
import             Linear

--import qualified   Graphics.Rendering.OpenGL       as GL

---------------------------------------------------------------------------------------------------

-- | sdf = shader def
-- sp = shader program
-- m the inside monad
class Monad m => MonadShader sdf sp m | m -> sdf, m -> sp where
    -- | set (.=) for an infix
    setUniform :: AsUniform u => (sdf -> UniformDef u m) -> u -> m ()
    enableAttrib :: (sdf -> AttributeDef vad m) -> m ()

newtype Shader d p m a = Shader { runShader :: d -> p -> m a }

instance Monad m => Monad (Shader d p m) where
    return a = Shader $ \d p -> return a
    m >>= k = Shader $ \d p -> do
        a <- runShader m d p
        runShader (k a) d p
    fail str = Shader $ \_ _ -> fail str

instance MonadTrans (Shader d p) where
    lift m = Shader $ \_ _ -> m

instance MonadIO m => MonadIO (Shader d p m) where
    liftIO = lift . liftIO


-- TODO: abstract p Program
instance (Monad m, p ~ Program) => MonadShader d p (Shader d p m) where
    setUniform loc value = Shader $ \d p -> let (_, action) = loc d in runShader (action p value) d p
    enableAttrib loc = Shader $ \d p -> let (attr, action) = loc d in runShader (action p attr) d p

type Program = ShaderProgram

data ShaderAttributes s vad = 
      VertexPos !s !vad
    | VertexNormal !s !vad

type EnableAction vad m = ShaderProgram -> ShaderAttributes String vad -> m ()
type AttributeDef vad m = (ShaderAttributes String vad, EnableAction vad m)

data ShaderUniforms s = 
      GlobalTime !s
    | ProjectionMatrix !s
    | ViewMatrix !s
    | ModelMatrix !s
    | NormalMatrix !s

type SetAction u m = ShaderProgram -> u -> m ()
type UniformDef u m = (ShaderUniforms String, SetAction u m)

data ShaderDefs vad m = ShaderDefs
    { sVertexPosition     :: AttributeDef vad m
    , sVertexNormal       :: AttributeDef vad m
    , sGlobalTime         :: (AsUniform u) => UniformDef u m
    , sProjectionMatrix   :: (AsUniform u) => UniformDef u m
    , sViewMatrix         :: (AsUniform u) => UniformDef u m
    , sModelMatrix        :: (AsUniform u) => UniformDef u m
    , sNormalMatrix       :: (AsUniform u) => UniformDef u m
    }

---------------------------------------------------------------------------------------------------

(.=) :: (AsUniform u, Monad m, MonadShader sdf sp m) => (sdf -> UniformDef u m) -> u -> m ()
(.=) = setUniform
