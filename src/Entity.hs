module Entity where

import Texture
import SDL
import Linear (V2(..))
import Foreign.C.Types (CInt)
import qualified Data.Map as Map

class Entity e where
  move             :: e -> [Event] -> CInt -> IO e
  getTextureConfig :: e -> TextureConfig
  getRectangle     :: e -> Rectangle CInt
  
  outOfBounds      :: e -> Bool
  outOfBounds ent = rightEdge < 0 where
    (Rectangle (P (V2 x _)) (V2 w _)) = getRectangle ent
    rightEdge = x + w

  render           :: Renderer -> TextureMap -> e -> CInt -> IO ()
  render renderer tMap ent currTime = do
    let config = getTextureConfig ent
    let (V2 startX startY) = (offset config)
    let (V2 fWidth _) = (frameSize config)
    let animationIdx = (currTime `div` (frameTime config)) `rem` (numFrames config)

    let framePos = V2 (startX + (animationIdx * fWidth)) startY
    let srcRect = Rectangle (P framePos) (frameSize config)

    let (Just tex) = Map.lookup (name config) tMap

    copy renderer tex (Just srcRect) (Just (getRectangle ent))

data EntityBase = EntityBase {
  rect    :: Rectangle CInt,
  speed   :: V2 Double,
  pos     :: V2 Double,
  texture :: TextureConfig
} deriving (Show, Eq)

instance Entity EntityBase where
  move e _ _ = return e
  getTextureConfig = texture
  getRectangle = rect