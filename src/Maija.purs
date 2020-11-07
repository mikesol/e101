module E101.Maija where

import Prelude
import Data.Array (range)
import Data.Int (toNumber)
import Data.Lazy (Lazy, defer, force)
import Data.List (List(..), fold)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, evalPiecewise, playBufWithOffset_, runInBrowser, speaker)
import Math (pow)
import Type.Klank.Dev (Klank, defaultEngineInfo, klank, makeBuffersKeepingCache)

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

boundPlayer :: Number -> Lazy (List (AudioUnit D2)) -> Number -> List (AudioUnit D2)
boundPlayer len a time = if (time) + kr >= 0.0 && time < (len) then force a else Nil

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

maijaChirpPlayer :: String -> Number -> Number -> List (AudioUnit D2)
maijaChirpPlayer tag rate = boundPlayer (0.5 / rate) (defer \_ -> pure (playBufWithOffset_ tag "maija" rate 1.3))

maijaGlissPlayer :: String -> Number -> Number -> List (AudioUnit D2)
maijaGlissPlayer tag rate = boundPlayer (2.5 / rate) (defer \_ -> pure (playBufWithOffset_ tag "maija" rate 4.3))

maijaSirenPlayer :: String -> Number -> Number -> List (AudioUnit D2)
maijaSirenPlayer tag rate = boundPlayer (2.5 / rate) (defer \_ -> pure (playBufWithOffset_ tag "maija" rate 8.2))

maijaGliss :: String -> Int -> Array (Number -> List (AudioUnit D2))
maijaGliss tag end =
  map
    ( \i ->
        let
          tn = toNumber i
        in
          atT (0.0 + tn * 0.1) $ maijaGlissPlayer (tag <> show i) (1.0 + tn * 0.05)
    )
    (range 0 (end - 1))

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    ( speaker
        ( zero
            :| ( fold
                  ( map (\f -> f time)
                      ( (map (atT 5.85) (maijaGliss "glissA" 7))
                          <> [ atT 0.0 $ maijaChirpPlayer "chirpA" 1.9
                            , atT 1.7 $ maijaChirpPlayer "chirpB" 1.4
                            , atT 3.4 $ maijaChirpPlayer "chirpC" 1.5
                            , atT 4.2 $ maijaChirpPlayer "chirpD" 1.2
                            , atT 5.5 $ maijaChirpPlayer "chirpE" 1.6
                            ]
                          <> [ atT 0.0 $ maijaSirenPlayer "sirenA" 0.25
                            ]
                      )
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
        [ Tuple "maija" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/maija0.ogg"
        ]
    }
