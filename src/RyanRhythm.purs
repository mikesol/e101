module Klank.Dev where

import Prelude
import Data.Array (range)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.NonEmpty ((:|))
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D1)
import Effect.Class (liftEffect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, Oversample(..), loopBuf, makeFloatArray, runInBrowser, speaker, waveShaper)
import Foreign.Object as O
import Math (pi)
import Type.Klank.Dev (Klank, affable, klank, makeBuffersKeepingCache)

makeDistortionCurve :: Number -> Array Number
makeDistortionCurve k =
  map
    ( \i ->
        let
          x = (toNumber i * 2.0 / toNumber n_samples) - 1.0
        in
          (3.0 + k) * x * 20.0 * deg / (pi + (k * abs x))
    )
    (range 0 $ n_samples - 1)
  where
  n_samples = 44100

  deg = pi / 180.0

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  pure
    $ speaker
        ( (waveShaper "wicked" FourX (loopBuf "rhythm" 1.0 0.65 3.65))
            :| Nil
        )
  where
  rad = time * pi

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , floatArrays =
      const
        $ affable
            ( do
                wicked <- liftEffect $ makeFloatArray (makeDistortionCurve 400.0)
                pure $ O.singleton "wicked" wicked
            )
    , buffers =
      makeBuffersKeepingCache
        [ Tuple "rhythm" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/ryanGliiitch.ogg"
        ]
    }
