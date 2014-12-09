{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Yage.Rendering.Resources.GL.Shader
  ( compileProgram
  , createShaderPipeline
  , shaderTypeToPipelineStage
  , fileToShaderType
  ) where

import           Quine.GL.ProgramPipeline
import           Yage.Core.OpenGL
import           Yage.Prelude
import           Yage.Rendering.Backend.Resource

import           Quine.GL                        as GL ()
import           Quine.GL.Program
import           Quine.GL.Shader


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

compileProgram :: MonadIO m => FilePath -> [FilePath] -> m (ShaderType, Program)
compileProgram fp paths = do
  let ty = fileToShaderType fp
  src <- readFile fp
  liftM (ty,) $ createShaderProgramInclude ty src (map fpToString paths)

createShaderPipeline :: [(PipelineStage, Program)] -> Acquire ProgramPipeline
createShaderPipeline stages = do
  pipeline <- glResource
  forM_ stages $ \(s,p) -> useProgramStages pipeline s p
  return pipeline
