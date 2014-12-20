{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}

module Yage.Rendering.Resources.GL.Shader
  ( ShaderProgram(..), shaderType, shaderProg, shaderLog, filePath
  , compileProgram
  , createShaderPipeline
  , compileShaderPipeline
  , shaderTypeToPipelineStage
  , fileToShaderType
  , validatePipeline
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
import           Quine.GL                        as GL ()
import           Quine.GL.Program
import           Quine.GL.Shader                 hiding (shaderType)
import           Quine.GL.Object
import           Quine.StateVar
import Language.Haskell.TH.Syntax

data ShaderProgram  = ShaderProgram
  { _filePath     :: FilePath
  , _shaderType   :: ShaderType
  , _shaderProg   :: Program
  , _shaderLog    :: [String]
  } deriving (Show,Ord,Eq,Data,Typeable,Generic)

makeLenses ''ShaderProgram

data ShaderException = ShaderException FilePath [String]
  deriving (Show,Eq,Data,Typeable,Generic)
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
  bsE <- (runIO $ readFile fp) >>= bsToExp
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
    unless linked $ throwM $ ShaderException fp shlog
    return $ ShaderProgram fp ty prog shlog
  free = delete . _shaderProg

createShaderPipeline :: [ShaderProgram] -> Acquire ProgramPipeline
createShaderPipeline programs = do
  pipeline <- glResource
  forM_ programs $ \prog -> useProgramStages pipeline (shaderTypeToPipelineStage $ prog^.shaderType) (prog^.shaderProg)
  return pipeline

compileShaderPipeline :: [(FilePath, ByteString)] -> [FilePath] -> Acquire ProgramPipeline
compileShaderPipeline files paths = createShaderPipeline =<< mapM (`compileProgram` paths) files

validatePipeline :: MonadIO m => ProgramPipeline -> m [String]
validatePipeline pipeline = liftM (fmap Char8.unpack . Char8.lines) (validateProgramPipeline pipeline >> programPipelineInfoLog pipeline)

