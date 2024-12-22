{-# LANGUAGE OverloadedStrings #-}

module Title (Title.render, loadTitleTextures, freeTitleTextures) where

import SDL
import qualified SDL.Image as SDLImage
import qualified SDL.Font as Font
import Linear (V2(..), V4(..))
import Foreign.C.Types (CInt)
import Control.Monad (unless)
import Entity
import Game
import Settings (screenWidth, screenHeight)
import Data.Text (pack)


freeTitleTextures :: [(Texture, Rectangle CInt)] -> IO ()
freeTitleTextures textureData = do
  mapM_ (\(tex, _) -> destroyTexture tex) textureData


loadTitleTextures :: Renderer -> String -> Font.Font -> IO ([(Texture, Rectangle CInt)])
loadTitleTextures renderer titleText font = do
  backgroundTexture <- SDLImage.loadTexture renderer "assets/title/title.png"

  textureInfo <- queryTexture backgroundTexture
  let backgroundWidth = textureWidth textureInfo
      backgroundHeight = textureHeight textureInfo

  let screenW = fromIntegral $ screenWidth
      screenH = fromIntegral $ screenHeight

  let backgroundRect = Rectangle (P $ V2 0 0) (V2 screenW screenH)

  copy renderer backgroundTexture Nothing (Just backgroundRect)

  surface <- Font.solid font (V4 255 255 255 255) (pack titleText)
  texture <- createTextureFromSurface renderer surface
  freeSurface surface
  (textWidth, textHeight) <- Font.size font (pack titleText)
  let textRect = Rectangle (P $ V2 (screenW `div` 2 - fromIntegral textWidth `div` 2) 100) 
                            (V2 (fromIntegral textWidth) (fromIntegral textHeight))

  copy renderer texture Nothing (Just textRect)

  hiScore <- loadHiScore
  let hiScoreText = pack $ "High Score: " ++ show hiScore 
  hiScoreSurface <- Font.solid font (V4 255 255 255 255) hiScoreText
  hiScoreTexture <- createTextureFromSurface renderer hiScoreSurface
  freeSurface hiScoreSurface
  (hiScoreWidth, hiScoreHeight) <- Font.size font hiScoreText
  let hiScoreRect = Rectangle (P $ V2 (screenW `div` 2 - fromIntegral hiScoreWidth `div` 2) 150) 
                            (V2 (fromIntegral hiScoreWidth) (fromIntegral textHeight))
  copy renderer hiScoreTexture Nothing (Just hiScoreRect)

  instructionSurface <- Font.solid font (V4 255 255 255 255) "Start [Enter] | Quit [Esc]"
  instructionTexture <- createTextureFromSurface renderer instructionSurface
  freeSurface instructionSurface
  (instrWidth, instrHeight) <- Font.size font "Press Enter to Start"
  let instructionRect = Rectangle (P $ V2 (screenW `div` 2 - fromIntegral instrWidth `div` 2) (screenHeight - fromIntegral instrHeight))
                                   (V2 (fromIntegral instrWidth) (fromIntegral instrHeight))
  copy renderer instructionTexture Nothing (Just instructionRect)

  return [(backgroundTexture, backgroundRect), (texture, textRect), (hiScoreTexture, hiScoreRect), (instructionTexture, instructionRect)]

renderText :: Renderer -> (Texture, Rectangle CInt) -> IO ()
renderText renderer (texture, dstRect) = do
  copy renderer texture Nothing (Just dstRect)

-- Render the home screen
render :: (Entity e) => GameData -> CInt -> e -> [(Texture, Rectangle CInt)] -> IO ()
render gd currTime snailEnt renderTuples = do
  clear (renderer gd)
  mapM_ (\tuple -> renderText (renderer gd) tuple) renderTuples
  Entity.render (renderer gd) (tMap gd) snailEnt currTime
  present (renderer gd)