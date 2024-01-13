{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Platformer.ECS (
    Color (..),
    Entity (..),
    Health (..),
    System (..),
    mkEmptyEntity,
    mkRenderSystem,
    setAppearance,
    setHealth,
    setPosition,
) where

import Control.Monad
import Data.UUID (UUID)
import Data.Word (Word8)
import Foreign.C.Types
import SDL (($=))
import SDL qualified
import System.Random (randomIO)

newtype Health = Health {value :: Int}
    deriving stock (Eq, Ord, Show)

data Color = Red | Green | Yellow | Blue
    deriving stock (Eq, Ord, Show)

toV4 :: Color -> SDL.V4 Word8
toV4 = \case
    Red -> SDL.V4 maxBound 0 0 100
    Green -> SDL.V4 0 maxBound 0 100
    Blue -> SDL.V4 0 0 maxBound 100
    Yellow -> SDL.V4 maxBound maxBound 0 maxBound

data Entity = Entity
    { id :: UUID
    , health :: Maybe Health
    , appearance :: Maybe Color
    , position :: Maybe (CInt, CInt)
    }
    deriving stock (Eq, Ord, Show)

mkEmptyEntity :: IO Entity
mkEmptyEntity = do
    uuid <- randomIO
    pure $ Entity uuid Nothing Nothing Nothing

setHealth :: Health -> Entity -> Entity
setHealth health entity = entity{health = Just health}

setAppearance :: Color -> Entity -> Entity
setAppearance color entity = entity{appearance = Just color}

setPosition :: (CInt, CInt) -> Entity -> Entity
setPosition position entity = entity{position = Just position}

newtype System = System {run :: [Entity] -> IO ()}

mkRenderSystem :: SDL.Renderer -> SDL.Texture -> System
mkRenderSystem renderer tileset = System $ \entities ->
    forM_ entities $ \e -> do
        case (e.health, e.appearance, e.position) of
            (Just health, Just color, Just (x, y)) -> do
                SDL.rendererDrawColor renderer $= toV4 color
                let rect = SDL.Rectangle (SDL.P $ SDL.V2 x y) (SDL.V2 10 10)
                SDL.fillRect renderer (Just rect)
            _ -> pure ()
