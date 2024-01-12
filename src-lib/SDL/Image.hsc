{-# LANGUAGE ForeignFunctionInterface #-}

module SDL.Image where

import Control.Exception.Safe qualified as Exception
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL qualified
import SDL.Raw.Types qualified as Raw

foreign import ccall unsafe "SDL_image IMG_Init"
    initRaw :: CInt -> IO CInt

foreign import ccall unsafe "SDL_image IMG_Quit"
    quitRaw :: IO ()

foreign import ccall unsafe "SDL_image IMG_Load"
    loadRaw :: CString -> IO (Ptr Raw.Surface)

loadSurface :: FilePath -> IO SDL.Surface
loadSurface path = do
    psurface <- withCString path loadRaw
    if psurface == nullPtr
        then fail $ "IMG_Load failed to handle " <> path
        else pure $ SDL.Surface psurface Nothing

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture renderer path =
    Exception.bracket
        (loadSurface path)
        SDL.freeSurface
        (SDL.createTextureFromSurface renderer)
