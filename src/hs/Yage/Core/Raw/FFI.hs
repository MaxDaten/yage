{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
module Yage.Core.Raw.FFI (
	  YApplication
	  , mkApplication, exec, processEvents
	, YGLWindow
	  , mkGLWindow, showWindow, beginDraw, endDraw, resizeWindow, height, width, pixelRatio
	) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array


data CApplication = CApplication
type YApplication = ForeignPtr CApplication
foreign import ccall safe "createApplication"   c_mkApplication                 :: CInt -> Ptr (CString) -> IO (Ptr CApplication)
foreign import ccall safe "&deleteApplication"  c_delApplication                :: FunPtr (Ptr CApplication -> IO ())
foreign import ccall safe "exec"                c_Application_exec              :: Ptr CApplication -> IO (CInt)
foreign import ccall safe "processEvents"       c_Application_processEvents     :: Ptr CApplication -> IO ()

---------------------------------------------------------------------------------------------------

data CGLWindow = CGLWindow
type YGLWindow = ForeignPtr CGLWindow
foreign import ccall safe "createGLWindow"      c_mkGLWindow                    :: IO (Ptr CGLWindow)
foreign import ccall safe "&deleteGLWindow"     c_delGLWindow                   :: FunPtr (Ptr CGLWindow -> IO ())
foreign import ccall safe "show"                c_GLWindow_show                 :: Ptr CGLWindow -> IO ()
foreign import ccall safe "endDraw"             c_GLWindow_endDraw              :: Ptr CGLWindow -> IO ()
foreign import ccall safe "beginDraw"           c_GLWindow_beginDraw            :: Ptr CGLWindow -> IO ()
foreign import ccall safe "resize"              c_GLWindow_resize               :: Ptr CGLWindow -> CInt -> CInt -> IO ()
foreign import ccall safe "width"               c_GLWindow_width                :: Ptr CGLWindow -> IO (CInt)
foreign import ccall safe "height"              c_GLWindow_height               :: Ptr CGLWindow -> IO (CInt)
foreign import ccall safe "pixelRatio"          c_GLWindow_pixelRatio           :: Ptr CGLWindow -> IO (CDouble)
foreign import ccall safe "makeCurrent"         c_GLWindow_makeCurrent          :: Ptr CGLWindow -> IO ()
---------------------------------------------------------------------------------------------------

mkApplication' :: [String] -> IO (Ptr CApplication)
mkApplication' args = do
    cargs <- mapM newCString args
    withArray cargs $ \argv -> c_mkApplication (cargc) argv
    where 
        cargc = fromIntegral $ length args
        
mkApplication :: [String] -> IO (ForeignPtr CApplication)
mkApplication args = mkApplication' args >>= newForeignPtr c_delApplication

exec :: YApplication -> IO (Int)
exec = doWithApplication $ \a -> return . fromIntegral =<< c_Application_exec a

processEvents :: YApplication -> IO ()
processEvents = doWithApplication c_Application_processEvents

withApplication :: YApplication -> (Ptr CApplication -> IO b) -> IO b
withApplication = withForeignPtr

doWithApplication :: (Ptr CApplication -> IO b) -> YApplication -> IO b
doWithApplication = flip withForeignPtr

---------------------------------------------------------------------------------------------------

mkGLWindow :: IO (ForeignPtr CGLWindow)
mkGLWindow = c_mkGLWindow >>= newForeignPtr c_delGLWindow

showWindow :: YGLWindow -> IO ()
showWindow = doWithWindow c_GLWindow_show

beginDraw :: YGLWindow -> IO ()
beginDraw = doWithWindow c_GLWindow_beginDraw

endDraw :: YGLWindow -> IO ()
endDraw = doWithWindow c_GLWindow_endDraw

resizeWindow :: YGLWindow -> Int -> Int -> IO ()
resizeWindow window width height = withWindow window $ \w -> c_GLWindow_resize w cWidth cHeight
    where cWidth = fromIntegral width
          cHeight = fromIntegral height

height :: YGLWindow -> IO (Int)
height = doWithWindow $ \w -> return . fromIntegral =<< c_GLWindow_height w

width :: YGLWindow -> IO (Int)
width = doWithWindow $ \w -> return . fromIntegral =<< c_GLWindow_width w

pixelRatio :: YGLWindow -> IO (Double)
pixelRatio = doWithWindow $ \w -> return . realToFrac =<< c_GLWindow_pixelRatio w

makeCurrent :: YGLWindow -> IO ()
makeCurrent = doWithWindow c_GLWindow_makeCurrent

withWindow :: YGLWindow -> (Ptr CGLWindow -> IO b) -> IO b
withWindow = withForeignPtr

doWithWindow :: (Ptr CGLWindow -> IO b) -> YGLWindow -> IO b
doWithWindow = flip withWindow

---------------------------------------------------------------------------------------------------

