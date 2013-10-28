module Yage.Wire where

import Yage.Prelude hiding (id, (.))
import Control.Wire
import Graphics.Rendering.OpenGL.GL (Color4(..))
import Data.Tuple.Curry
import Text.Printf

import Yage.Rendering.Types
import Yage.Types


type ColorChs = (Double, Double, Double, Double)

countFrame :: (Monad m) => Wire e m a Int
countFrame = countFrom 0 <<< 1


timeString :: (Monad m) => Wire e m a String
timeString = fmap (printf "%8.2f") time

impure :: (Monad m, Functor m) => (a -> m b) -> Wire e m a b
impure f = mkFixM $ \_ x -> Right <$> f x

showW :: (MonadIO m, Functor m, Show a) => Wire e m a a
showW = impure (\x -> liftIO (print x) >> return x )

-- this traces only if value is eval'd
traceW :: (Show a) => Wire e m a a
traceW = mkFix $ \_dt x -> Right (traceShow x x)


--initWith = ((produce . once) <|> empty) . keep


-- | high order wire
-- produces if argument wire produces
-- kepp result of argument wire and keep it forever
-- inhibits forever if argument wire inhibits
produceOnce :: Monad m => Wire e m a b -> Wire e m a b
produceOnce w' = mkGen $ \dt x' -> do
	(mx, _) <- stepWire w' dt x'
	return (mx, mkFixM $ const . const $ return mx)


---------------------------------------------------------------------------------------------------


-- f: channel manipulation function
colorTransformW :: (Double -> Double) -> YageWire ColorChs (Color4 Double)
colorTransformW f = arr (fmap f) . colorW

-- | in: Speed-Vector
--   out: integrated color over time |sin|
colorW :: YageWire ColorChs (Color4 Double)
colorW = arr (uncurryN Color4) . integral_ (0, 0, 0, 0)

clearColorW :: YageWire (Color4 Double) ()
clearColorW = mkFixM $ \_ c -> do
    rConf <- getRenderConfig
    putRenderConfig rConf{ confClearColor = c }
    return $ Right ()

