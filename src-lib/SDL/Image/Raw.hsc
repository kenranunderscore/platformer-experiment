{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module SDL.Image.Raw (
    initialize,
    load,
    pattern IMG_INIT_AVIF,
    pattern IMG_INIT_JPG,
    pattern IMG_INIT_JXL ,
    pattern IMG_INIT_PNG,
    pattern IMG_INIT_TIF,
    pattern IMG_INIT_WEBP,
    quit,
)
where

#include "SDL_image.h"

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Types qualified as Raw

foreign import ccall unsafe "SDL_image IMG_Init"
    initialize :: CInt -> IO CInt

foreign import ccall unsafe "SDL_image IMG_Quit"
    quit :: IO ()

foreign import ccall unsafe "SDL_image IMG_Load"
    load :: CString -> IO (Ptr Raw.Surface)

pattern IMG_INIT_JPG = #{const IMG_INIT_JPG}
pattern IMG_INIT_PNG = #{const IMG_INIT_PNG}
pattern IMG_INIT_TIF = #{const IMG_INIT_TIF}
pattern IMG_INIT_WEBP = #{const IMG_INIT_WEBP}
pattern IMG_INIT_JXL = #{const IMG_INIT_JXL}
pattern IMG_INIT_AVIF = #{const IMG_INIT_AVIF}
