import qualified SDL
import Player
import Entity
import Foreign.C.Types (CInt)
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ testGroup "Tests" $
  [ testCase "Player creation" $ do
      let p = createPlayer
      (speed (pBase p)) @=? SDL.V2 0.0 0.0
  ]
