{-# LANGUAGE OverloadedStrings #-}

module Platformer.Main (main) where

import Control.Exception.Safe qualified as Exception
import Control.Monad
import Foreign.C.Types (CInt)
import SDL (($=))
import SDL qualified
import SDL.Image qualified as Image

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
withSdlWindow w h action = do
    let windowConfig = SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 w h}
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
        let
            source = Tiles.getRect tile
            dest =
                SDL.Rectangle
                    (SDL.P $ SDL.V2 (Tiles.tileSize * fromIntegral x) (Tiles.tileSize * fromIntegral y))
                    Tiles.tileDimensions
        in
            SDL.copy renderer tileset (Just source) (Just dest)

gameLoop renderer world tileset = do
    evts <- SDL.pollEvents
    SDL.clear renderer
    renderWorld renderer world tileset
    SDL.present renderer
    unless (any escPressed evts) (gameLoop renderer world tileset)
  where
    escPressed evt =
        case SDL.eventPayload evt of
            SDL.KeyboardEvent keyboardEvent ->
                SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                    && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeEscape
            _ -> False

main :: IO ()
main = do
    let globalScale = 20
    world <- World.load "test.world"
    withSdl
        $ withSdlWindow
            (globalScale * Tiles.tileSize * World.width world)
            (globalScale * Tiles.tileSize * World.height world)
        $ \window -> do
            withSdlImage [Image.PNG] $ do
                withSdlRenderer window $ \renderer -> do
                    tileset <- Tiles.loadTileset renderer
                    SDL.rendererScale renderer $= SDL.V2 (realToFrac globalScale) (realToFrac globalScale)
                    gameLoop renderer world tileset
