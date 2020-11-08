module E101.Election where

import Prelude
import Data.Int (toNumber)
import Data.List (List(..), fold)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, evalPiecewise, gainT_', playBufWithOffset_, runInBrowser, speaker)
import Math (pow)
import Type.Klank.Dev (Klank, defaultEngineInfo, klank, makeBuffersKeepingCache)

tempo = 60.0 :: Number

q = 60.0 / tempo :: Number

_8 = 60.0 / (2.0 * tempo) :: Number

_16 = 60.0 / (4.0 * tempo) :: Number

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

boundPlayer :: Number -> (Number -> List (AudioUnit D2)) -> Number -> List (AudioUnit D2)
boundPlayer len a time = if (time) + kr >= 0.0 && time < (len) then a time else Nil

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

ep :: String -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
ep tag len rate os =
  boundPlayer len
    ( \t ->
        pure
          ( gainT_'
              (tag <> "gain")
              (epwf [ Tuple 0.0 1.0, Tuple (len - 0.03) 1.0, Tuple len 0.0 ] t)
              (playBufWithOffset_ (tag <> "buf") "election" rate os)
          )
    )

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    ( speaker
        ( zero
            :| ( fold
                  ( map (\f -> f time)
                      [ atT 0.0 $ ep "a" q 1.0 0.0 ]
                  )
              )
        )
    )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers =
      makeBuffersKeepingCache
        [ Tuple "election" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/election.ogg"
        ]
    }
