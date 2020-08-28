module PCM where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as B

data PlayerState a = PlayerState {
                      pitchStandard :: a,
                      tone :: a -> a,
                      tempo :: Integer,
                      sampleRate :: a,
                      volume :: a,
                      builder :: a -> B.Builder
                   }

defaultPlayerState :: PlayerState Float
defaultPlayerState = PlayerState { pitchStandard = 440
                                 , tone          = sin
                                 , tempo         = 100
                                 , sampleRate    = 32000
                                 , volume        = 0.4
                                 , builder       = B.floatLE
                                 }

semitone :: Floating a => PlayerState a -> Int -> a
semitone ps n = (pitchStandard ps) * (2 ** (1 / 12)) ** fromIntegral n

note :: (Floating a, Enum a) => PlayerState a -> (a, Int, Bool) -> B.Builder
note ps (beat, semi, rest) = foldMap
  (waveEq ps)
  [0.0 .. (sampleRate ps) * beatLen * beat]
 where
  beatLen = (60 / (fromIntegral . tempo) ps)
  noteMul = 2 * pi * semitone ps semi / sampleRate ps
  vol     = if rest then 0 else volume ps
  waveEq ps = builder ps . (*) vol . tone ps . (*) noteMul
