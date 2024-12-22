module Texture where

import SDL
import SDL.Image
import Linear(V2(..))
import Foreign.C.Types (CInt)
import qualified Data.Map as Map

type TextureMap = Map.Map String Texture

allTextures :: [(String, FilePath)]
allTextures = [ ("stickbug", "assets/enemy/stickbug.png"),
                ("bird",     "assets/enemy/bird.png"),
                ("snail",    "assets/snail.png"),
                ("tile",     "assets/tileset.png"),
                ("bg1",      "assets/bg/bg1.png"),
                ("bg2",      "assets/bg/bg2.png"),
                ("bg3",      "assets/bg/bg3.png"),
                ("bg4",      "assets/bg/bg4.png"),
                ("bg5",      "assets/bg/bg5.png")
              ]

loadTextureMap :: Renderer -> IO TextureMap
loadTextureMap renderer = do
  textures <- mapM (\(_, path) -> loadTexture renderer path) allTextures
  return $ Map.fromList (zip (map fst allTextures) textures)

data TextureConfig = TextureConfig {
  frameSize    :: V2 CInt,
  frameTime    :: CInt,
  numFrames    :: CInt,

  offset       :: V2 CInt,
  name         :: String,
  size         :: V2 CInt
} deriving (Eq, Show)

snailConfig :: TextureConfig
snailConfig = TextureConfig {
  frameSize = V2 24 24,
  frameTime = 120,
  numFrames = 3,
  offset    = V2 0 0,
  name      = "snail",
  size      = V2 72 24
}

stickbugConfig :: TextureConfig
stickbugConfig = TextureConfig {
  frameSize = V2 80 64,
  frameTime = 120,
  numFrames = 8,
  offset    = V2 0 0,
  name      = "stickbug",
  size      = V2 640 64
}

birdConfig :: TextureConfig
birdConfig = TextureConfig {
  frameSize = V2 48 48,
  frameTime = 120,
  numFrames = 7,
  offset    = V2 0 0,
  name      = "bird",
  size      = V2 336 48
}

tileConfig :: TextureConfig
tileConfig = TextureConfig {
  frameSize = V2 16 48,
  frameTime = 0,
  numFrames = 0,
  offset    = V2 96 32,
  name      = "tile",
  size      = V2 16 48
}