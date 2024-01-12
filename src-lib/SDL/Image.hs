{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module SDL.Image (
    InitFlag (..),
    initialize,
    loadTexture,
    quit,
) where

import Control.Exception.Safe qualified as Exception
import Control.Monad
import Data.Bits ((.|.))
import Data.Foldable (foldl')
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL qualified
import SDL.Image.Raw qualified as Raw

data InitFlag
    = JPG
    | PNG
    | TIF
    | WEBP
    | JXL
    | AVIF
    deriving stock (Enum, Bounded, Show, Eq)

flagToCInt :: InitFlag -> CInt
flagToCInt = \case
    JPG -> Raw.IMG_INIT_JPG
    PNG -> Raw.IMG_INIT_PNG
    TIF -> Raw.IMG_INIT_TIF
    WEBP -> Raw.IMG_INIT_WEBP
    JXL -> Raw.IMG_INIT_JXL
    AVIF -> Raw.IMG_INIT_AVIF

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

initialize :: [InitFlag] -> IO ()
initialize flags = do
    let val = foldl' (\res flag -> res .|. flagToCInt flag) 0 flags
    void $ Raw.initialize val

quit :: IO ()
quit = Raw.quit
