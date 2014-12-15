{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}

module Yage.Rendering.Resources.GL.Shader
  ( ShaderProgram(..), shaderType, shaderProg, shaderLog
  , compileProgram
  , createShaderPipeline
  , shaderTypeToPipelineStage
  , fileToShaderType
  , validatePipeline
  ) where

import           Yage.Prelude
import           Yage.Lens
import           Quine.GL.ProgramPipeline
import           Yage.Rendering.GL
import           Yage.Rendering.Resources.GL.Base
import qualified Data.ByteString.Char8 as Char8
import           Data.Data

import           Quine.GL                        as GL ()
import           Quine.GL.Program
import           Quine.GL.Shader                 hiding (shaderType)
import           Quine.GL.Object
import           Quine.StateVar

data ShaderProgram  = ShaderProgram
  { _shaderType   :: ShaderType
  , _shaderProg   :: Program
  , _shaderLog    :: [String]
  } deriving (Show,Ord,Eq,Data,Typeable,Generic)

makeLenses ''ShaderProgram

data ShaderException = ShaderException [String] deriving (Show,Eq,Data,Typeable,Generic)
instance Exception ShaderException

fileToShaderType :: FilePath -> ShaderType
fileToShaderType fp
  | hasExtension fp "vert"  = GL_VERTEX_SHADER
  | hasExtension fp "tessc" = GL_TESS_CONTROL_SHADER
  | hasExtension fp "tesse" = GL_TESS_EVALUATION_SHADER
  | hasExtension fp "geo"   = GL_GEOMETRY_SHADER
  | hasExtension fp "frag"  = GL_FRAGMENT_SHADER
  | hasExtension fp "comp"  = GL_COMPUTE_SHADER
  | otherwise = error "invalid shader extension"

shaderTypeToPipelineStage :: ShaderType -> PipelineStage
shaderTypeToPipelineStage = \case
  GL_VERTEX_SHADER          -> GL_VERTEX_SHADER_BIT
  GL_TESS_CONTROL_SHADER    -> GL_TESS_CONTROL_SHADER_BIT
  GL_TESS_EVALUATION_SHADER -> GL_TESS_EVALUATION_SHADER_BIT
  GL_GEOMETRY_SHADER        -> GL_GEOMETRY_SHADER_BIT
  GL_FRAGMENT_SHADER        -> GL_FRAGMENT_SHADER_BIT
  GL_COMPUTE_SHADER         -> GL_COMPUTE_SHADER_BIT
  _                         -> error "unknown ShaderType"

compileProgram :: FilePath -> [FilePath] -> Acquire ShaderProgram
compileProgram fp paths = mkAcquire create free where
  create = do
    let ty = fileToShaderType fp
    src <- readFile fp
    s <- createShader ty
    shaderSource s $= src
    compileShaderInclude s (fpToString <$> paths)
    compiled <- compileStatus s
    prog <- gen
    when compiled $ do
      programSeparable prog $= True
      attachShader prog s
      linkProgram prog
      detachShader prog s
    shlog <- liftM2 (++) (fmap Char8.unpack . Char8.lines <$> shaderInfoLog s)
                         (fmap Char8.unpack . Char8.lines <$> programInfoLog prog)
    delete s
    linked <- linkStatus prog
    unless linked $ throwM $ ShaderException shlog
    return $ ShaderProgram ty prog shlog
  free = delete . _shaderProg

createShaderPipeline :: [ShaderProgram] -> Acquire ProgramPipeline
createShaderPipeline programs = do
  pipeline <- glResource
  forM_ programs $ \prog -> useProgramStages pipeline (shaderTypeToPipelineStage $ prog^.shaderType) (prog^.shaderProg)
  return pipeline

validatePipeline :: MonadIO m => ProgramPipeline -> m [String]
validatePipeline pipeline = liftM (fmap Char8.unpack . Char8.lines) (validateProgramPipeline pipeline >> programPipelineInfoLog pipeline)

