module Background where

import Game
import SDL
import Settings (screenHeight, screenWidth)
import Texture
import Linear (V2(..))
import Foreign.C.Types (CInt)
import qualified Data.Map as Map


bgWidth, bgHeight :: CInt
bgWidth = 384
bgHeight = 216


renderBackground :: GameData -> CInt -> IO ()
renderBackground gd currTime =
    mapM_ (\(name, rate) -> addLayer gd name rate currTime) (zip names rates) where
    names = ["bg1", "bg2", "bg3", "bg4", "bg5"]
    rates = [0, 25, 50, 75, 100]


addLayer :: GameData -> String -> CInt -> CInt -> IO ()
addLayer gd tName rate currTime = do
    let rend = (renderer gd)
    let texMap = (tMap gd)
    let (Just tex) = Map.lookup tName texMap

    let bWidth = bgWidth
    let bHeight = bgHeight
    let sWidth = screenWidth
    let sHeight = screenHeight

    let offset = (currTime * rate `div` 1000) `mod` bWidth

    let src1 = Rectangle (P (V2 offset 0)) (V2 (bWidth - offset) bHeight)
        dst1 = Rectangle (P (V2 0 0))
                         (V2 ((sWidth * (bWidth - offset)) `div` bWidth) sHeight)

    let src2 = Rectangle (P (V2 0 0)) (V2 offset bHeight)
        dst2 = Rectangle (P (V2 (fromIntegral $ ((sWidth * (bWidth - offset)) `div` bWidth)) 0))
                            (V2 ((sWidth * offset) `div` bWidth) sHeight)

    copy rend tex (Just src1) (Just dst1)
    copy rend tex (Just src2) (Just dst2)  


renderFloor :: GameData -> IO ()
renderFloor gd =
  mapM_ (\dst -> copy rend tex (Just src) (Just dst)) dstList where
  rend = (renderer gd)
  texMap = (tMap gd)
  config = tileConfig

  (Just tex) = Map.lookup (name config) texMap
  (V2 tW tH) = (frameSize config)
  src = Rectangle (P (offset config)) (V2 tW tH)
  lastIndex = (screenWidth `div` tW) * tW
  dstList = [Rectangle (P (V2 idx (screenHeight - tH))) (V2 tW tH) | idx <- [0,tW..lastIndex]]