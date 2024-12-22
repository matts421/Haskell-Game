{-# LANGUAGE OverloadedStrings #-}

module Main where

import Player
import Music
import Settings
import Title
import Game
import Entity
import qualified Data.Map as Map
import Texture
    ( TextureConfig(frameTime), snailConfig, loadTextureMap )
import Background (renderBackground, renderFloor)
import UI (renderScores)

import Data.Text (Text, pack)
import SDL
import qualified SDL.Image as SDLImage
import qualified SDL.Font as Font
import qualified SDL.Mixer
import Linear (V2(..))
import Linear (V4(..))
import Control.Monad (unless, when)
import Foreign.C.Types (CInt)

initSDLResources :: IO ()
initSDLResources = do
  initializeAll
  initializeAudioPlayer
  Font.initialize

createGameData :: IO GameData
createGameData = do
  window <- createWindow "A Snail's Adventure!" gameWindowConfig
  renderer <- createRenderer window (-1) defaultRenderer
  textureMap <- loadTextureMap renderer

  return GameData {
    window   = window,
    renderer = renderer,
    tMap     = textureMap
  }

freeSDLResources :: GameData -> IO ()
freeSDLResources gameData = do
  destroyRenderer (renderer gameData)
  destroyWindow (window gameData)


titleLoop :: GameData -> [(Texture, Rectangle CInt)] -> IO Bool
titleLoop gd textureData = do
  events <- pollEvents
  let escapePress = anyKeyPressed [KeycodeEscape] events
  let enterPress = anyKeyPressed [KeycodeReturn] events

  let snailEnt = EntityBase {
    rect = Rectangle (P (V2 (screenWidth `div` 2 - 200) (screenHeight `div` 2 - 200))) (V2 400 400),
    pos = V2 0.0 0.0,
    speed = V2 0.0 0.0,
    texture = snailConfig {
      frameTime = 360
    }
  }
  currTicks <- SDL.ticks
  Title.render gd (wordToCInt currTicks) snailEnt textureData

  if      escapePress then do return False
  else if enterPress  then do return True
  else titleLoop gd textureData


main :: IO ()
main = do
  initSDLResources
  gameData <- createGameData

  font <- Font.load "assets/title/font.ttf" 40
  bg <- SDL.Mixer.load "music/bg-loop.ogg"
  SDL.Mixer.playMusic SDL.Mixer.Forever bg

  playGame gameData "A Snail's Adventure!" font

  Font.free font
  freeSDLResources gameData

playGame :: GameData -> String -> Font.Font -> IO ()
playGame gameData titleText font = do
  textureData <- loadTitleTextures (renderer gameData) titleText font
  shouldPlay <- titleLoop gameData textureData

  freeTitleTextures textureData

  if shouldPlay then do
    playSound "music/sfx/confirm.wav"
    initTime <- SDL.ticks
    gameState <- createGameState (wordToCInt initTime)
    finalScore <- appLoop gameData gameState

    playSound "music/sfx/game-over.wav"
    SDL.delay 3000
    
    hiScore <- loadHiScore
    when ((finalScore `div` 1000) > hiScore) $ saveHiScore (finalScore `div` 1000)
    
    playGame gameData "Game Over" font
  
  else do
    playSound "music/sfx/cancel.wav"
    SDL.delay 100

renderGame :: GameData -> GameState -> IO ()
renderGame gameData gameState = do
  rendererDrawColor (renderer gameData) $= V4 0 0 0 255
  clear (renderer gameData)

  let currTime = (Game.time gameState)

  renderBackground gameData currTime
  renderFloor gameData
  renderGameState gameData gameState
  
  font <- Font.load "assets/title/font.ttf" 20
  renderScores gameData gameState font
  Font.free font

  present (renderer gameData)

appLoop :: GameData -> GameState -> IO CInt
appLoop gameData gameState = do
    events <- pollEvents

    currTime <- SDL.ticks
    gameState' <- updateGameState gameData gameState events

    renderGame gameData gameState'

    if (dead gameState') then
        return $ Game.time gameState' 
    else
        appLoop gameData gameState' 


-- anyKeyPressed :: [Keycode] -> [Event] -> Bool
-- anyKeyPressed keys events = any (keyPressed keys) events

keyPressed :: [Keycode] -> Event -> Bool
keyPressed keys (Event _ (KeyboardEvent (KeyboardEventData _ _ _ keysym))) = 
    keysymKeycode keysym `elem` keys
keyPressed _ _ = False

