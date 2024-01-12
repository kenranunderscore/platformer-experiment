module SDL.Image (
    initialize,
    loadTexture,
) where

import Control.Exception.Safe qualified as Exception
import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL qualified

import SDL.Image.Raw qualified as Raw

loadSurface :: FilePath -> IO SDL.Surface
loadSurface path = do
    psurface <- withCString path Raw.load
    if psurface == nullPtr
        then fail $ "IMG_Load failed to handle " <> path
        else pure $ SDL.Surface psurface Nothing

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture renderer path =
    Exception.bracket
        (loadSurface path)
        SDL.freeSurface
        (SDL.createTextureFromSurface renderer)

initialize :: CInt -> IO ()
initialize = void . Raw.initialize
