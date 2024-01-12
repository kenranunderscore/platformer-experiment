{-# LANGUAGE OverloadedStrings #-}

module Platformer.Main (main) where

import Control.Exception.Safe qualified as Exception
import Control.Monad
import Foreign.C.Types (CInt)
import SDL qualified
import SDL.Image qualified as Image

tileSize :: CInt
tileSize = 4

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

withSdlWindow :: (SDL.Window -> IO a) -> IO a
withSdlWindow action = do
    let windowConfig = SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 (160 * tileSize) (144 * tileSize)}
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

gameLoop renderer tex = do
    evts <- SDL.pollEvents
    SDL.clear renderer
    SDL.copy renderer tex Nothing Nothing
    SDL.present renderer
    unless (any escPressed evts) (gameLoop renderer tex)
  where
    escPressed evt =
        case SDL.eventPayload evt of
            SDL.KeyboardEvent keyboardEvent ->
                SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                    && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeEscape
            _ -> False

main :: IO ()
main = do
    withSdl $ withSdlWindow $ \window -> do
        withSdlImage [Image.PNG] $ do
            withSdlRenderer window $ \renderer -> do
                tex <- Image.loadTexture renderer "tileset.png"
                gameLoop renderer tex
