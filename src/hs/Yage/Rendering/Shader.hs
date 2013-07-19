{-# LANGUAGE ExistentialQuantification, Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, StandaloneDeriving, TypeFamilies #-}
module Yage.Rendering.Shader where

import             Yage.Import
import             Control.Monad                 (liftM)
import             Control.Monad.IO.Class
import             Control.Monad.Trans

import             Graphics.GLUtil               (ShaderProgram, asUniform, getUniform)
import             Graphics.GLUtil.Linear        (AsUniform)
import             Linear

import qualified   Graphics.Rendering.OpenGL       as GL


-- | sdf = shader def
-- sp = shader program
-- m the inside monad
class Monad m => MonadShader sdf sp m | m -> sdf, m -> sp where
    (.=) :: AsUniform u => (sdf -> UniformDef u m) -> u -> m ()

newtype Shader d p m a = Shader { runShader :: d -> p -> m a }

--deriving instance Monad (Shader d p m a)

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


--type ShaderP d p a m = Shader d p m a
-- abstract ShaderProgram
instance (Monad m, p ~ Program) => MonadShader d p (Shader d p m) where
    loc .= uni = Shader $ \d p -> runShader ((snd . loc $ d) p uni) d p -- weierdo


data ShaderAttributes = VertexPos String

data ShaderUniforms = 
      GlobalTime String
    | ProjectionMatrix String
    | ViewMatrix String
    | ModelMatrix String

type Program = ShaderProgram
type SetAction u m = ShaderProgram -> u -> m ()
type UniformDef u m = (ShaderUniforms, SetAction u m)

-- move to a generic def format
data ShaderDefs m = ShaderDefs
    { sGlobalTime         :: UniformDef GL.GLfloat m
    , sProjectionMatrix   :: UniformDef (M44 GL.GLfloat) m
    , sViewMatrix         :: UniformDef (M44 GL.GLfloat) m
    , sModelMatrix        :: UniformDef (M44 GL.GLfloat) m
    }


