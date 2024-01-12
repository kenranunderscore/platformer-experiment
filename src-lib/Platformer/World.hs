{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Platformer.World (
    World (..),
    coordMap_,
    height,
    load,
    width,
) where

import Data.Vector (Vector)
import Data.Vector qualified as V

import Platformer.Tiles

newtype World = World {value :: Vector (Vector Tile)}
    deriving (Show)

width :: Integral a => World -> a
width world = fromIntegral . length $ V.head world.value

height :: Integral a => World -> a
height world = fromIntegral $ length world.value

coordMap_ :: Monad m => ((Int, Int) -> Tile -> m a) -> World -> m ()
coordMap_ f world =
    V.imapM_
        (\y -> V.imapM_ (\x -> f (x, y)))
        world.value

load :: FilePath -> IO World
load path = do
    rows <- lines <$> readFile path
    tiles <-
        V.fromList
            <$> mapM
                ( fmap V.fromList
                    . mapM
                        ( \case
                            ' ' -> pure Empty
                            'x' -> pure Solid
                            unknown -> fail $ "unknown tile character: " <> [unknown]
                        )
                )
                rows
    pure $ World tiles
