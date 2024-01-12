{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module SDL.Image (
    InitFlag (..),
    initialize,
    loadTexture,
) where

import Control.Exception.Safe qualified as Exception
import Control.Monad
import Data.Bits ((.<<.), (.|.))
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

-- FIXME: use constants from header file (reexported)
flagToCInt :: InitFlag -> CInt
flagToCInt = \case
    JPG -> 1
    PNG -> 1 .<<. 1
    TIF -> 1 .<<. 2
    WEBP -> 1 .<<. 3
    JXL -> 1 .<<. 4
    AVIF -> 1 .<<. 5

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
