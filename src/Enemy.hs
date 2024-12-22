{-# LANGUAGE InstanceSigs #-}
module Enemy where

import SDL
import Entity
import Settings (screenWidth, screenHeight, floorY, cIntToDouble)
import Texture (TextureConfig, stickbugConfig, birdConfig)
import System.Random (randomRIO)
import Control.Monad (when, void)
import Foreign.C.Types (CInt)

birdSpeed, stickbugSpeed :: Double
birdSpeed = 700.0
stickbugSpeed = 500.0

data EnemyType = StickBug | Bird deriving (Eq, Show)

data Enemy = Enemy {
    eType :: EnemyType,
    eBase :: EntityBase
} deriving(Eq, Show)

instance Entity Enemy where
  getRectangle = rect . eBase
  getTextureConfig = texture . eBase

  move :: Enemy -> [Event] -> CInt -> IO Enemy
  move enemy _ deltaTime = do
    let base = (eBase enemy)
    let dtS = (fromIntegral deltaTime :: Double) / 1000.0

    let (V2 dblX dblY) = (pos base)
    let (V2 moveSpeed _) = (speed base)

    let (Rectangle (P (V2 intX intY)) size) = (rect base)
    
    let nDblX = dblX - (dtS * moveSpeed)
    let nIntX = fromIntegral (floor nDblX) :: CInt

    let newRect = (Rectangle (P (V2 nIntX intY)) size)

    return $ enemy{eBase = base{rect = newRect, pos= V2 nDblX dblY}}


createStickBug :: CInt -> CInt -> Double -> Enemy
createStickBug xVal _ scale = Enemy {
    eType = StickBug,
    eBase = base
} where
    scaledSpeed = scale * stickbugSpeed
    base = EntityBase {
        rect    = (Rectangle (P (V2 xVal (floorY - 170))) (V2 240 192)),
        speed   = V2 scaledSpeed 0.0,
        texture = stickbugConfig,
        pos     = V2 (cIntToDouble xVal) (cIntToDouble (floorY - 170))
    }

createBird :: CInt -> CInt -> Double -> Enemy
createBird xVal yVal scale = Enemy {
    eType = Bird,
    eBase = base
} where
    scaledSpeed = scale * birdSpeed
    base = EntityBase {
        rect    = (Rectangle (P (V2 xVal yVal)) (V2 144 144)),
        speed   = V2 scaledSpeed 0.0,
        texture = birdConfig,
        pos     = V2 (cIntToDouble xVal) (cIntToDouble yVal)
    }

createEnemy :: EnemyType -> CInt -> CInt -> Double -> Enemy
createEnemy enemyType xVal yVal scale =
    case enemyType of
        Bird     -> createBird xVal yVal scale
        StickBug -> createStickBug xVal yVal scale

spawnEnemy :: CInt -> CInt -> CInt -> IO (Maybe (CInt, Enemy))
spawnEnemy diff lastTime currTime = do
    let dtS = (currTime - lastTime) `div` 1000
    let spawnInterval = max 1 (5 - diff)  -- Enemy spawns every 5 seconds on lvl 0

    if dtS >= spawnInterval then do
        eTypeFlip <- randomRIO (0.0, 1.0) :: IO Double

        -- Only spawn birds after lvl 1
        let enemyType = if (diff >= 1) && (eTypeFlip >= 0.6) then Bird
                        else StickBug

        xVal <- randomRIO (screenWidth, screenWidth + (screenWidth `div` 8)) :: IO CInt
        yVal <- case enemyType of
                    Bird -> randomRIO (0, (2 * screenHeight) `div` 3) :: IO CInt
                    StickBug -> return floorY
        speedScale <- randomRIO (0.8, 1.2) :: IO Double

        return $ Just (currTime, (createEnemy enemyType xVal yVal speedScale))
    else do
        return Nothing