module FFI where

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
foreign import ccall safe "createGLWindow"  c_mkGLWindow            :: IO (Ptr (CGLWindow))
foreign import ccall safe "&deleteGLWindow"     c_delGLWindow           :: FunPtr (Ptr (CGLWindow) -> IO ())
foreign import ccall safe "show"                c_GLWindow_show         :: Ptr (CGLWindow) -> IO ()
foreign import ccall safe "endDraw"             c_GLWindow_endDraw      :: Ptr (CGLWindow) -> IO ()
foreign import ccall safe "beginDraw"       c_GLWindow_beginDraw    :: Ptr (CGLWindow) -> IO ()
foreign import ccall safe "resize"          c_GLWindow_resize       :: Ptr (CGLWindow) -> CInt -> CInt -> IO ()
foreign import ccall safe "width"           c_GLWindow_width        :: Ptr (CGLWindow) -> IO (CInt)
foreign import ccall safe "height"          c_GLWindow_height       :: Ptr (CGLWindow) -> IO (CInt)
foreign import ccall safe "pixelRatio"      c_GLWindow_pixelRatio   :: Ptr (CGLWindow) -> IO (CDouble)
foreign import ccall safe "makeCurrent"     c_GLWindow_makeCurrent  :: Ptr (CGLWindow) -> IO ()
---------------------------------------------------------------------------------------------------
-- really needed?
foreign import ccall unsafe "realMain"      c_main                  :: Ptr CGLWindow -> Ptr CApplication -> IO ()
--foreign import ccall safe "&cdelete" c_delete :: FunPtr (Ptr a -> IO ())

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
exec app = withForeignPtr app $ \a -> return . fromIntegral =<< c_Application_exec a

processEvents :: YApplication -> IO ()
processEvents app = withForeignPtr app c_Application_processEvents

withApplication :: YApplication -> (Ptr CApplication -> IO b) -> IO b
withApplication = withForeignPtr

---------------------------------------------------------------------------------------------------

mkGLWindow :: IO (ForeignPtr CGLWindow)
mkGLWindow = c_mkGLWindow >>= newForeignPtr c_delGLWindow

showWindow :: YGLWindow -> IO ()
showWindow = flip withForeignPtr $ c_GLWindow_show

beginDraw :: YGLWindow -> IO ()
beginDraw = flip withForeignPtr $ c_GLWindow_beginDraw

endDraw :: YGLWindow -> IO ()
endDraw = flip withForeignPtr $ c_GLWindow_endDraw

resizeWindow :: YGLWindow -> Int -> Int -> IO ()
resizeWindow window width height = withForeignPtr window $ \w -> c_GLWindow_resize w cWidth cHeight
    where cWidth = fromIntegral width
          cHeight = fromIntegral height

height :: YGLWindow -> IO (Int)
height window = withForeignPtr window $ \w -> return . fromIntegral =<< c_GLWindow_height w

width :: YGLWindow -> IO (Int)
width window = withForeignPtr window $ \w -> return . fromIntegral =<< c_GLWindow_width w

pixelRatio :: YGLWindow -> IO (Double)
pixelRatio window = withForeignPtr window $ \w -> return . realToFrac =<< c_GLWindow_pixelRatio w

makeCurrent :: YGLWindow -> IO ()
makeCurrent = flip withForeignPtr $ c_GLWindow_makeCurrent

withWindow :: YGLWindow -> (Ptr CGLWindow -> IO b) -> IO b
withWindow = withForeignPtr

---------------------------------------------------------------------------------------------------

--callMain :: IO ()
--callMain = do 
--  c_main (fromIntegral 0) (nullPtr)
--  return ()

---------------------------------------------------------------------------------------------------
