module E101.Wild where

import Prelude
import Data.Array (replicate)
import Data.Int (toNumber)
import Data.List (List(..), foldl)
import Data.Profunctor (lcmap)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import Debug.Trace (spy)
import Effect.Random (random)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, convolver_, dup2, evalPiecewise, g'add, g'delay, g'gain, gainT_', gain_', graph, loopBufT_, loopBuf_, playBufWithOffset_, runInBrowser, runInBrowser_, speaker')
import Math (pi, pow, sin)
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

scene :: Array (Tuple Number Number) -> Number -> Behavior (AudioUnit D2)
scene a time =
  pure
    $ speaker'
        (loopBufT_ ("rhythm") "rhythm" (epwf a time) 0.5 4.0)

aggT ::
  Array (Tuple Number Number) ->
  { acc :: Array (Tuple Number Number), len :: Number }
aggT =
  foldl
    (\{ acc, len } (Tuple n x) -> { acc: acc <> [ (Tuple (n + len) x) ], len: len + n })
    { acc: [], len: 0.0 }

main :: Klank
main =
  klank
    { run =
      runInBrowser_ do
        rands <-
          sequence
            ( replicate 1000
                ( do
                    l <- random
                    r <- random
                    pure (Tuple l r)
                )
            )
        let
          mapped =
            map
              (\(Tuple l r) -> (Tuple (l + 0.03) (0.6 + r * 4.0)))
              rands
        let
          accd = aggT mapped
        pure (scene accd.acc)
    , buffers =
      makeBuffersKeepingCache
        [ Tuple "rhythm" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/ryanRhythm.ogg"
        ]
    }
