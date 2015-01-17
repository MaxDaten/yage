{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Yage.Rendering.Resources.GL.Shader
  ( ShaderProgram(..), shaderType, shaderProg, shaderLog, filePath
  , Pipeline(..), pipelineProgram, pipelinePrograms
  , compileProgram
  , createShaderPipeline
  , compileShaderPipeline
  , shaderTypeToPipelineStage
  , fileToShaderType
  , validatePipeline
  , checkPipelineError
  -- * Embeding Shader Sources
  , embedShaderFile
  ) where

import           Yage.Prelude
import           Yage.Lens
import           Quine.GL.ProgramPipeline
import           Yage.Rendering.GL
import           Yage.Rendering.Resources.GL.Base
import qualified Data.ByteString.Char8 as Char8
import           Data.Data
import           Data.FileEmbed (bsToExp)
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import           Quine.GL                        as GL ()
import           Quine.GL.Program
import           Quine.GL.Shader                 hiding (shaderType)
import           Quine.GL.Object
import           Quine.StateVar
import Language.Haskell.TH.Syntax

data ShaderProgram = ShaderProgram
  { _filePath     :: !FilePath
  , _shaderType   :: !ShaderType
  , _shaderProg   :: !Program
  , _shaderLog    :: ![String]
  } deriving (Show,Ord,Eq,Data,Typeable,Generic)

makeLenses ''ShaderProgram

data Pipeline = Pipeline
  { _pipelineProgram  :: !ProgramPipeline
  , _pipelinePrograms :: ![ShaderProgram]
  } deriving (Show,Ord,Eq,Data,Typeable,Generic)

makeLenses ''Pipeline

data ShaderException =
    ShaderException String FilePath [String]
  | PipelineValidationError Pipeline [String]
  deriving (Eq,Show,Data,Typeable,Generic)

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


-- | Embed a file as a pair of name and
-- > fragment :: (FilePath, ByteString)
-- > fragment = $(embedShaderFile "glsl/color.frag")
embedShaderFile :: FilePath -> Q Exp
embedShaderFile fp = do
  qAddDependentFile $ fpToString fp
  typ <- [t| (FilePath, ByteString) |]
  bsE <- bsToExp . T.encodeUtf8 =<< (runIO $ T.readFile (fpToString fp))
  fpE <- filePathToExp fp
  return $ SigE (TupE [fpE, bsE]) typ

filePathToExp :: FilePath -> Q Exp
filePathToExp fp = do
  helper <- [| fpFromString |]
  let cs = fpToString fp
  return $! AppE helper $! LitE $! StringL cs

compileProgram :: (FilePath, ByteString) -> [FilePath] -> Acquire ShaderProgram
compileProgram (fp, src) paths = mkAcquire create free where
  create = do
    let ty = fileToShaderType fp
    s <- createShader ty
    shaderSource s $= repack src
    compileShaderInclude s (fpToString <$> paths)
    compiled <- compileStatus s
    shaderLog <- fmap (force.Char8.unpack) . Char8.lines <$> shaderInfoLog s
    prog <- gen
    when compiled $ do
      programSeparable prog $= True
      attachShader prog s
      linkProgram prog
      detachShader prog s
    theLog <- liftM ((++) shaderLog) (fmap (force.Char8.unpack) . Char8.lines <$> programInfoLog prog)
    delete s
    linked <- linkStatus prog
    unless linked $ throwM $ ShaderException "linking not successful" fp theLog
    return $ ShaderProgram fp ty prog theLog
  free = delete . _shaderProg

createShaderPipeline :: [ShaderProgram] -> Acquire Pipeline
createShaderPipeline programs = do
  pipeline <- glResource
  forM_ programs $ \prog -> useProgramStages pipeline (shaderTypeToPipelineStage $ prog^.shaderType) (prog^.shaderProg)
  return $ Pipeline pipeline programs

compileShaderPipeline :: [(FilePath, ByteString)] -> [FilePath] -> Acquire Pipeline
compileShaderPipeline files paths = createShaderPipeline =<< mapM (`compileProgram` paths) files

validatePipeline :: MonadIO m => Pipeline -> m [String]
validatePipeline Pipeline{..} = do
  let shlog = concat $! _pipelinePrograms^..traversed.shaderLog
  validateProgramPipeline _pipelineProgram
  pipeLog <- liftM (fmap (force.Char8.unpack) . Char8.lines) (programPipelineInfoLog _pipelineProgram)
  return $! pipeLog ++ shlog

-- | Throws a 'PipelineValidationError' when the validation is not successful (log contains entries) and
-- the compiler 'GL_ERRCHECK' flag is defined
checkPipelineError :: (MonadThrow m, MonadIO m) => Pipeline -> m ()
#ifdef GL_ERRCHECK
checkPipelineError p = validatePipeline p >>= \l -> unless (null l) $ throwM $ PipelineValidationError p l
#else
checkPipelineError = const (return ())
#endif
