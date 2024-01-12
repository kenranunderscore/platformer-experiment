{-# LANGUAGE ForeignFunctionInterface #-}

module SDL.Image.Raw (
    initialize,
    quit,
    load,
)
where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL qualified
import SDL.Raw.Types qualified as Raw

foreign import ccall unsafe "SDL_image IMG_Init"
    initialize :: CInt -> IO CInt

foreign import ccall unsafe "SDL_image IMG_Quit"
    quit :: IO ()

foreign import ccall unsafe "SDL_image IMG_Load"
    load :: CString -> IO (Ptr Raw.Surface)
