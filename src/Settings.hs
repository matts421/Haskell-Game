module Settings where
-- Module for static game settings like sprite sizes and SDL config

import SDL
import Linear (V2(..))
import Foreign.C.Types (CInt)
import GHC.Word (Word32)


anyKeyPressed :: [Keycode] -> [Event] -> Bool
anyKeyPressed keycodes events =
  any (\event -> case eventPayload event of
                   KeyboardEvent keyboardEvent ->
                     keyboardEventKeyMotion keyboardEvent == Pressed &&
                     keysymKeycode (keyboardEventKeysym keyboardEvent) `elem` keycodes
                   _ -> False)
      events

wordToCInt :: Word32 -> CInt
wordToCInt = fromIntegral . toInteger

cIntToDouble :: CInt -> Double
cIntToDouble val = fromIntegral val :: Double

screenWidth, screenHeight, floorY :: CInt
screenWidth = 1152
screenHeight = 648
floorY = screenHeight - 48

gameWindowConfig :: WindowConfig
gameWindowConfig = WindowConfig {
  windowBorder          = True,
  windowHighDPI         = False,
  windowInputGrabbed    = False,
  windowMode            = Windowed,
  windowGraphicsContext = NoGraphicsContext,
  windowPosition        = Wherever,
  windowResizable       = False,
  windowInitialSize     = V2 screenWidth screenHeight,
  windowVisible         = True
}



