{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Platformer.Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception.Safe qualified as Exception
import Control.Monad
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Foreign.C.Types (CInt)
import SDL (($=))
import SDL qualified
import SDL.Image qualified as Image

import Platformer.ECS qualified as ECS
import Platformer.Tiles qualified as Tiles
import Platformer.World qualified as World

withSdl :: IO a -> IO a
withSdl =
    Exception.bracket_
        ( do
            SDL.initialize [SDL.InitVideo]
            putStrLn "initialized SDL"
        )
        ( do
            SDL.quit
            putStrLn "shut down SDL"
        )

withSdlImage :: [Image.InitFlag] -> IO a -> IO a
withSdlImage flags =
    Exception.bracket_
        ( do
            Image.initialize flags
            putStrLn "initialized SDL_image"
        )
        ( do
            Image.quit
            putStrLn "quit SDL_image"
        )

withSdlWindow :: CInt -> CInt -> (SDL.Window -> IO a) -> IO a
withSdlWindow width height action = do
    let windowConfig = SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 width height}
    Exception.bracket
        ( do
            putStrLn "creating SDL window..."
            SDL.createWindow "platformer" windowConfig
        )
        ( \w -> do
            SDL.destroyWindow w
            putStrLn "window destroyed"
        )
        action

withSdlRenderer :: SDL.Window -> (SDL.Renderer -> IO a) -> IO a
withSdlRenderer window = do
    let rendererConfig = SDL.defaultRenderer{SDL.rendererType = SDL.AcceleratedVSyncRenderer}
    Exception.bracket
        ( do
            putStrLn "creating SDL renderer..."
            SDL.createRenderer window (-1) rendererConfig
        )
        ( \r -> do
            SDL.destroyRenderer r
            putStrLn "renderer destroyed"
        )

renderWorld :: SDL.Renderer -> World.World -> SDL.Texture -> IO ()
renderWorld renderer world tileset =
    World.coordMap_ renderTile world
  where
    renderTile (x, y) tile =
        let source = Tiles.getRect tile
            dest =
                SDL.Rectangle
                    (SDL.P $ SDL.V2 (Tiles.tileSize * fromIntegral x) (Tiles.tileSize * fromIntegral y))
                    Tiles.tileDimensions
        in SDL.copy renderer tileset (Just source) (Just dest)

escPressed evt =
    case SDL.eventPayload evt of
        SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeEscape
        _ -> False

updateInputState :: MVar ECS.InputState -> [SDL.KeyboardEventData] -> IO ()
updateInputState inputState keyboardEvents =
    forM_ keyboardEvents $ \evt -> do
        let val = SDL.keyboardEventKeyMotion evt == SDL.Pressed
        case SDL.keysymScancode (SDL.keyboardEventKeysym evt) of
            -- FIXME: collect keydowns/ups and build state before updating
            SDL.ScancodeDown -> modifyMVar_ inputState (\s -> pure s{ECS.down = val})
            SDL.ScancodeUp -> modifyMVar_ inputState (\s -> pure s{ECS.up = val})
            SDL.ScancodeLeft -> modifyMVar_ inputState (\s -> pure s{ECS.left = val})
            SDL.ScancodeRight -> modifyMVar_ inputState (\s -> pure s{ECS.right = val})
            _ -> pure ()

gameLoop ::
    SDL.Renderer ->
    World.World ->
    SDL.Texture ->
    MVar ECS.InputState ->
    [ECS.System] ->
    [ECS.Entity] ->
    IO ()
gameLoop renderer world tileset inputState systems =
    go
  where
    go entities = do
        evts <- SDL.pollEvents
        updateInputState inputState $
            mapMaybe
                ( \e -> case e.eventPayload of
                    SDL.KeyboardEvent payload -> Just payload
                    _ -> Nothing
                )
                evts
        SDL.rendererDrawColor renderer $= SDL.V4 20 20 25 150
        SDL.clear renderer
        entities' <- foldM (\es sys -> sys.run es) entities systems
        threadDelay 10_000
        SDL.present renderer
        unless (any escPressed evts) (go entities')

mkFoo :: IO ECS.Entity
mkFoo =
    ECS.mkEmptyEntity <&> ECS.setHealth (ECS.Health 100)

main :: IO ()
main = do
    let globalScale = 5
    world <- World.load "test.world"
    withSdl $
        withSdlWindow 2000 1800 $
            \window -> do
                withSdlImage [Image.PNG] $ do
                    withSdlRenderer window $ \renderer -> do
                        tileset <- Tiles.loadTileset renderer
                        inputState <- newMVar ECS.initialInputState
                        SDL.rendererScale renderer $= SDL.V2 (realToFrac globalScale) (realToFrac globalScale)
                        let renderSystem = ECS.mkRenderSystem renderer tileset
                            inputSystem = ECS.mkInputSystem (readMVar inputState)
                            systems = [inputSystem, renderSystem]
                        player <-
                            ECS.setPlayerControlled
                                . ECS.setPosition (20, 20)
                                . ECS.setAppearance ECS.Yellow
                                <$> mkFoo
                        foo1 <- ECS.setPosition (80, 50) . ECS.setAppearance ECS.Red <$> mkFoo
                        foo2 <- ECS.setPosition (0, 90) . ECS.setAppearance ECS.Red <$> mkFoo
                        let entities = [player, foo1, foo2]
                        gameLoop renderer world tileset inputState systems entities
