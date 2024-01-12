{-# LANGUAGE DerivingStrategies #-}

module Platformer.Tiles (
    Tile (..),
    getRect,
    loadTileset,
) where

import Foreign.C.Types (CInt)
import SDL qualified
import SDL.Image qualified as Image

data Tile = Empty | Solid
    deriving stock (Show, Enum, Bounded, Eq)

tileSize :: CInt
tileSize = 8

-- FIXME: cache this
getRect :: Tile -> SDL.Rectangle CInt
getRect tile =
    SDL.Rectangle (SDL.P $ SDL.V2 (tileSize * index) 0) (SDL.V2 tileSize tileSize)
  where
    index = fromIntegral $ fromEnum tile

loadTileset :: SDL.Renderer -> IO SDL.Texture
loadTileset renderer = Image.loadTexture renderer "tileset.png"
