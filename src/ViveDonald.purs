module E101.Wild where

import Prelude
import Data.Int (toNumber)
import Data.List (List(..), fold, (:))
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, bandpass_, convolver_, gain_, gain_', loopBuf_, panner_, playBuf_, runInBrowser, speaker)
import Math (cos, pi, sin, (%))
import Type.Klank.Dev (Klank, defaultEngineInfo, klank, makeBuffersKeepingCache)

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

boundPlayer :: forall a. Number -> (Number -> List a) -> Number -> List a
boundPlayer len a time = if (time) + kr >= 0.0 && time < (len) then a time else Nil

loop :: forall a. Number -> (Number -> a) -> (Number -> a)
loop t = lcmap (_ % t)

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( (gain_' "sing" 0.8 (loopBuf_ ("mbox") "mbox" (0.8 + 0.1 * sin (rad * 0.2)) 0.0 0.0))
            :| (panner_ "pn0" (0.5 * sin (rad * 0.2)) (gain_' "sing2" 0.2 (loopBuf_ ("mbox2") "mbox" (0.8 + 0.1 * cos (rad * 0.2)) 0.0 0.0)))
            : (panner_ "pn1" (-0.5 * sin (rad * 0.2)) (gain_' "sing3" 0.3 (bandpass_ "bp" (1500.0 + 1000.0 * sin (0.2 * rad)) 1.0 (loopBuf_ ("mbox3") "mbox" (0.8 + (-0.1) * sin (rad * 0.2)) 0.0 0.0))))
            : ( convolver_ "cv" "verb"
                  ( gain_ "gvb" 1.0
                      ( zero
                          :| ( fold
                                $ map (\f -> f time)
                                    [ loop (9.0) (atT 1.0 $ boundPlayer 3.0 $ (\t -> pure (playBuf_ "piki" "piki" 1.0)))
                                    ]
                            )
                      )
                  )
              )
            : Nil
        )
  where
  rad = pi * time

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers =
      makeBuffersKeepingCache
        [ Tuple "piki" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/piki1.ogg"
        , Tuple "mbox" "https://freesound.org/data/previews/195/195355_98926-lq.mp3"
        , Tuple "verb" "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/Impulses/matrix-reverb3.wav"
        ]
    }
