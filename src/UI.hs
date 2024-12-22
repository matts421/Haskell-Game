module UI where

import SDL
import qualified SDL.Font as Font
import Data.Text (pack)
import Foreign.C.Types (CInt)
import Game

renderScores :: GameData -> GameState -> Font.Font -> IO ()
renderScores gd gs font = do
    -- Score
    let scoreText = pack $ "Score: " ++ show ((Game.time gs) `div` 1000)
    scoreSurface <- Font.solid font (V4 255 255 255 255) scoreText
    scoreTexture <- createTextureFromSurface (renderer gd) scoreSurface
    freeSurface scoreSurface
    (scoreWidth, scoreHeight) <- Font.size font scoreText

    let scoreRect = Rectangle (P $ V2 10 10)
                            (V2 (fromIntegral scoreWidth) (fromIntegral scoreHeight))

    copy (renderer gd) scoreTexture Nothing (Just scoreRect)
    destroyTexture scoreTexture

    -- High Score
    let hiScoreText = pack $ "High Score: " ++ show (Game.hiScore gs)
    hiScoreSurface <- Font.solid font (V4 255 255 255 255) hiScoreText
    hiScoreTexture <- createTextureFromSurface (renderer gd) hiScoreSurface
    freeSurface hiScoreSurface
    (hiScoreWidth, hiScoreHeight) <- Font.size font hiScoreText

    let hiScoreRect = Rectangle (P $ V2 10 (20 + fromIntegral scoreHeight))
                            (V2 (fromIntegral hiScoreWidth) (fromIntegral hiScoreHeight))

    copy (renderer gd) hiScoreTexture Nothing (Just hiScoreRect)
    destroyTexture hiScoreTexture