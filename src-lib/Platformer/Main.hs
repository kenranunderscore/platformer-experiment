{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Platformer.Main (main) where

import Control.Exception.Safe qualified as Exception
import Control.Monad
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace
import Foreign.C.Types (CInt)
import SDL qualified
import SDL.Image qualified as IMG

import Platformer.World qualified as World

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

withSdlWindow :: (SDL.Window -> IO a) -> IO a
withSdlWindow action = do
    let windowConfig = SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 (160 * tileSize) (144 * tileSize)}
    Exception.bracket
        (SDL.createWindow "platformer" windowConfig)
        ( \w -> do
            SDL.destroyWindow w
            putStrLn "window destroyed"
        )
        action

withSdlRenderer :: SDL.Window -> (SDL.Renderer -> IO a) -> IO a
withSdlRenderer window = do
    let rendererConfig = SDL.defaultRenderer{SDL.rendererType = SDL.AcceleratedVSyncRenderer}
    Exception.bracket
        (SDL.createRenderer window (-1) rendererConfig)
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
    print =<< IMG.initRaw 3
    withSdl $ withSdlWindow $ \window -> do
        withSdlRenderer window $ \renderer -> do
            tex <- IMG.loadTexture renderer "tileset.png"
            gameLoop renderer tex
