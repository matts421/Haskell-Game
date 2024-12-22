{-# LANGUAGE InstanceSigs #-}
module Player where

import SDL
import Entity
import Music
import Texture (TextureConfig, snailConfig)
import Settings (screenWidth, floorY, cIntToDouble, anyKeyPressed)
import Control.Monad (when, void)
import Foreign.C.Types (CInt)

gravity, playerSpeed :: Double
gravity = 2000.0
playerSpeed = 1250.0

data Player = Player {
  pBase           :: EntityBase,
  minY            :: CInt,
  inAir           :: Bool
}

createPlayer :: Player
createPlayer = do
  let (V2 pWidth pHeight) = V2 64 64
  let yVal = floorY - pHeight
  let xVal = (screenWidth `div` 8)

  let base = EntityBase {
    rect = Rectangle (P (V2 xVal yVal)) (V2 pWidth pHeight),
    speed = V2 0.0 0.0,
    texture = snailConfig,
    pos   = V2 (cIntToDouble xVal) (cIntToDouble yVal)
  }
  
  Player {pBase = base, minY = yVal, inAir = False}
    

instance Entity Player where
  getRectangle = rect . pBase
  getTextureConfig = texture . pBase

  move :: Player -> [SDL.Event] -> CInt -> IO Player
  move player events deltaTime = do
    let base = (pBase player)
    let dtS = (fromIntegral deltaTime :: Double) / 1000.0

    let (V2 dblX dblY) = (pos base)
    let (V2 _ jumpSpeed) = (speed base)
    let (Rectangle (P (V2 intX intY)) size) = (rect base)

    let airborne = (inAir player) 
    let jumpPressed = (anyKeyPressed [KeycodeUp, KeycodeSpace] events)

    if      (not airborne && not jumpPressed) then do
      return player
    else if (not airborne && jumpPressed) then do
      -- Was on ground, time to jump
      playSound "music/sfx/jump.wav"

      let yVelo = -playerSpeed

      let ndblY = dblY + (dtS * yVelo)
      let ndblYV = yVelo + (dtS * gravity)
      let nintY = fromIntegral (round ndblY) :: CInt

      return $ player {
        pBase = base{
          rect = Rectangle (P (V2 intX nintY)) size,
          pos = V2 dblX ndblY,
          speed = V2 0.0 ndblYV
        },
        inAir = True
      }

    else do
      -- Was not on ground, time to fall
      let yVelo = jumpSpeed

      let ndblY = dblY + (dtS * yVelo)
      let ndblYV = yVelo + (dtS * gravity)
      let nintY = fromIntegral (round ndblY) :: CInt

      if nintY < (minY player) then do
        return $ player {
            pBase = base {
              rect  = Rectangle (P (V2 intX nintY)) size,
              pos   = V2 dblX ndblY,
              speed = V2 0.0 ndblYV
            },
            inAir = True
          }
      else do
        return $ player {
            pBase = base {
              rect  = Rectangle (P (V2 intX (minY player))) size,
              pos   = V2 dblX (cIntToDouble (minY player)),
              speed = V2 0.0 0.0
            },
            inAir = False
          }

