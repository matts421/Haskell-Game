module Music where

import qualified SDL.Mixer

allSounds :: [(String, FilePath)]
allSounds = [ ("confirm",   "music/sfx/confirm.wav"),
              ("jump",      "music/sfx/jump.wav"),
              ("level-up",  "music/sfx/level-up.wav"),
              ("game-over", "music/sfx/game-over.wav")
            ]

playSound :: FilePath -> IO SDL.Mixer.Channel
playSound path = do
  sound <- SDL.Mixer.load path
  SDL.Mixer.playOn SDL.Mixer.AllChannels SDL.Mixer.Once sound

initializeAudioPlayer :: IO ()
initializeAudioPlayer = do
  let myAudioConfig = SDL.Mixer.Audio {
    SDL.Mixer.audioFrequency = 44100,
    SDL.Mixer.audioFormat = SDL.Mixer.FormatS16_LSB,
    SDL.Mixer.audioOutput = SDL.Mixer.Stereo
  }
  SDL.Mixer.openAudio myAudioConfig 1024

