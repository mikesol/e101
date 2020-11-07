module E101.RyanGrowl where

import Prelude
import Color (rgb)
import Data.Array (fold, index, length, range)
import Data.Int (floor, toNumber)
import Data.List (List(..), fold, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, convolver_, dup1, dup2, evalPiecewise, g'add, g'delay, g'gain, gain', gainT_, gainT_', gain_', graph, panner_, playBufWithOffset_, runInBrowser, speaker')
import Graphics.Drawing (Drawing, fillColor, filled, rectangle)
import Math (cos, pi, pow, sin, (%))
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))
import Type.Klank.Dev (Klank, defaultEngineInfo, klank, makeBuffersKeepingCache)

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

boundPlayer :: forall a. Number -> (Number -> List a) -> Number -> List a
boundPlayer len a time = if (time) + kr >= 0.0 && time < (len) then a time else Nil

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker'
        ( dup2
            ( graph
                { aggregators:
                    { out: Tuple g'add (SLProxy :: SLProxy ("combine" :/ SNil))
                    , combine: Tuple g'add (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                    , gain: Tuple (g'gain 0.9) (SLProxy :: SLProxy ("del" :/ SNil))
                    }
                , processors:
                    { del: Tuple (g'delay (0.7 + 0.45 * sin (0.25 * time * pi))) (SProxy :: SProxy "mic")
                    }
                , generators:
                    { mic: playBufWithOffset_ ("growl") "growl" 1.0 0.0
                    }
                }
            ) \d ->
            ( (gainT_' "gn" (epwf [ Tuple 0.0 0.8, Tuple 8.9 0.8, Tuple 9.0 0.0 ] time) d) + (gain_' "gnx" 0.5 (convolver_ ("cv") "verb" d))
            )
        )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers =
      makeBuffersKeepingCache
        [ Tuple "growl" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/ryanSandpaper.ogg"
        , Tuple "verb" "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/Impulses/matrix-reverb3.wav"
        ]
    }
