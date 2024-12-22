module Game where

import Texture
import Music
import Settings
import Entity
import Player
import Enemy
import System.Directory (doesFileExist)

import qualified SDL
import Foreign.C.Types (CInt)
import Control.Monad (when, void)
import qualified Data.Map as Map

data GameData = GameData {
  window   :: SDL.Window,
  renderer :: SDL.Renderer,
  tMap     :: TextureMap
}

data GameState = GameState {
    player     :: Player,
    enemies    :: [Enemy],
    prevTime   :: CInt,
    time       :: CInt,
    difficulty :: CInt,
    hiScore    :: CInt,
    lastSpawn  :: CInt,
    initTime   :: CInt,
    dead       :: Bool
}

hiScorePath :: FilePath
hiScorePath = "assets/hiScore"

createGameState :: CInt -> IO GameState
createGameState currTime = do
    hiScore <- loadHiScore -- Extract the high score from the IO action
    return GameState {
        player     = createPlayer,
        enemies    = [],
        prevTime   = 0,
        time       = 0,
        difficulty = 0,
        hiScore    = hiScore, -- Use the extracted value here
        lastSpawn  = 0,
        initTime   = currTime,
        dead       = False
    }



-- Update difficulty every 20s, capping at 10
updateDifficulty :: CInt -> CInt -> IO CInt
updateDifficulty currDiff timeMs = do
    let timeS = timeMs `div` 1000
    let inc = 20
    let newDiff = min 10 (timeS `div` inc)
    
    when (newDiff == currDiff + 1) $ do
        void (playSound "music/sfx/level-up.wav")
    
    return $ newDiff
    
loadHiScore :: IO CInt
loadHiScore = do
    exists <- doesFileExist hiScorePath
    if exists then do
        hiScore <- readFile hiScorePath
        return $ fromIntegral (read hiScore :: Int)
    else return 0

saveHiScore :: CInt -> IO ()
saveHiScore hiScore = writeFile hiScorePath (show hiScore)


rectIntersect :: CInt -> CInt -> SDL.Rectangle CInt -> SDL.Rectangle CInt -> Bool
rectIntersect xPad yPad (SDL.Rectangle (SDL.P (SDL.V2 x1 y1)) (SDL.V2 w1 h1))
               (SDL.Rectangle (SDL.P (SDL.V2 x2 y2)) (SDL.V2 w2 h2)) =     
  not (x1 + w1        <= (x2 + xPad) || -- rect1 is entirely to the left of rect2
       x2 + w2 - xPad <= x1          || -- rect2 is entirely to the left of rect1
       y1 + h1        <= (y2 + yPad) || -- rect1 is entirely above rect2
       y2 + h2 - yPad <= y1)            -- rect2 is entirely above rect1


collision :: Player -> [Enemy] -> Bool
collision player enemies =
    any (\enemy -> rectIntersect 85 50 pRect (rect $ eBase enemy)) enemies where
    pRect = rect $ pBase player


updateEnemies :: GameState -> IO (CInt, [Enemy])
updateEnemies gs = do
    let deltaTime = (time gs) - (prevTime gs)

    newEs <- mapM (\e -> move e [] deltaTime) (enemies gs)
    spawnE <- spawnEnemy (difficulty gs) (lastSpawn gs) (time gs)
    let newEs' = filter (\e -> not $ outOfBounds e) newEs

    case spawnE of
        Just (nSpawn, newE) ->
            return $ (nSpawn, newE : newEs')
        Nothing ->
            return $ (lastSpawn gs, newEs')


updateGameState :: GameData -> GameState -> [SDL.Event] -> IO GameState
updateGameState gd gs events = do
    let deltaTime = (time gs) - (prevTime gs)
    let collide = collision (player gs) (enemies gs)
    let currScore = time gs `div` 1000

    newP <-  move (player gs) events deltaTime
    (nSpawn, newEs) <- updateEnemies gs
    newDiff <- updateDifficulty (difficulty gs) (time gs)

    newTick <- SDL.ticks
    let newTime = (wordToCInt newTick) - (initTime gs)
    
    let newHiScore = max (hiScore gs) currScore

    return gs{
        player     = newP,
        enemies    = newEs,
        prevTime   = (time gs),
        time       = newTime,
        difficulty = newDiff,
        hiScore    = newHiScore,
        lastSpawn  = nSpawn,
        dead       = collide
    }

renderGameState :: GameData -> GameState -> IO ()
renderGameState gd gs = do
    render (renderer gd) (tMap gd) (player gs) (time gs)
    mapM_ (\e -> render (renderer gd) (tMap gd) e (time gs)) (enemies gs)
