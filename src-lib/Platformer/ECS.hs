{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Platformer.ECS (
    Color (..),
    Entity (..),
    Health (..),
    InputState (..),
    System (..),
    initialInputState,
    mkEmptyEntity,
    mkInputSystem,
    mkRenderSystem,
    setAppearance,
    setHealth,
    setPlayerControlled,
    setPosition,
) where

import Control.Concurrent.MVar
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
    , playerControlled :: Bool
    }
    deriving stock (Eq, Ord, Show)

mkEmptyEntity :: IO Entity
mkEmptyEntity = do
    uuid <- randomIO
    pure $ Entity uuid Nothing Nothing Nothing False

setHealth :: Health -> Entity -> Entity
setHealth health entity = entity{health = Just health}

setAppearance :: Color -> Entity -> Entity
setAppearance color entity = entity{appearance = Just color}

setPosition :: (CInt, CInt) -> Entity -> Entity
setPosition position entity = entity{position = Just position}

setPlayerControlled :: Entity -> Entity
setPlayerControlled entity = entity{playerControlled = True}

newtype System = System {run :: [Entity] -> IO [Entity]}

mkRenderSystem :: SDL.Renderer -> SDL.Texture -> System
mkRenderSystem renderer tileset = System $ \entities ->
    forM entities $ \e -> do
        case (e.health, e.appearance, e.position) of
            (Just health, Just color, Just (x, y)) -> do
                SDL.rendererDrawColor renderer $= toV4 color
                let rect = SDL.Rectangle (SDL.P $ SDL.V2 x y) (SDL.V2 10 10)
                SDL.fillRect renderer (Just rect)
            _ -> pure ()
        pure e

data InputState = InputState
    { up :: Bool
    , down :: Bool
    , left :: Bool
    , right :: Bool
    }

initialInputState :: InputState
initialInputState = InputState False False False False

movementKeyPressed :: InputState -> Bool
movementKeyPressed s = s.up || s.down || s.left || s.right

mkInputSystem :: IO InputState -> System
mkInputSystem readInputState = System $ \entities -> do
    inputState <- readInputState
    if movementKeyPressed inputState
        then do
            forM entities $ \e ->
                case (e.playerControlled, e.position) of
                    (True, Just (x, y)) -> do
                        -- FIXME: dummy implementation only so far
                        let x'
                                | inputState.left = if inputState.right then x else x - 1
                                | inputState.right = x + 1
                                | otherwise = x
                        let y'
                                | inputState.up = if inputState.down then y else y - 1
                                | inputState.down = y + 1
                                | otherwise = y
                        pure $ setPosition (x', y') e
                    _ -> pure e
        else pure entities
