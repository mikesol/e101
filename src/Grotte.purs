module E101.Grotte where

import Prelude
import Data.Array (mapWithIndex)
import Data.Int (toNumber)
import Data.List (List(..), fold, (:))
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D2)
import Data.Vec ((+>), empty)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, EngineInfo, constant_, convolver_, evalPiecewise, g'add_, g'delay_, g'gain_, gainT_, gainT_', gain_, gain_', graph_, highpass_, makePeriodicWave, pannerMonoT_, pannerMono_, panner_, periodicOsc_, playBufWithOffset_, runInBrowser, speaker')
import Foreign.Object as O
import Math (cos, pi, pow, sin, (%))
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))
import Type.Klank.Dev (Klank, defaultEngineInfo, klank, makeBuffersKeepingCache)

engineInfo =
  defaultEngineInfo
    { msBetweenSamples = 40
    , msBetweenPings = 35
    , rewindUpperBound = 5.0
    } ::
    EngineInfo

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

loop :: forall a. Number -> (Number -> a) -> (Number -> a)
loop t = lcmap (_ % t)

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

boundPlayer :: forall a. Number -> (Number -> List a) -> Number -> List a
boundPlayer len a time = if (time) + kr >= 0.0 && time < (len) then a time else Nil

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

type PWF
  = Array (Tuple Number Number)

playerBackground :: String -> (Number -> PWF) -> (Number -> PWF) -> (Number -> Number) -> (Number -> Number) -> Number -> List (AudioUnit D2)
playerBackground tag panf gainf ratef pitchf =
  let
    len = 4.2
  in
    boundPlayer (len + 2.0)
      ( \t ->
          pure
            ( pannerMonoT_
                (tag <> "panner")
                (epwf (panf len) t)
                ( gainT_
                    (tag <> "gain")
                    (epwf (gainf len) t)
                    ( highpass_ (tag <> "hp") (1600.0 + 500.0 * sin (pi * t * 0.3)) 1.0
                        ( playBufWithOffset_
                            (tag <> "buf")
                            "background"
                            (ratef t)
                            1.3
                        )
                        :| (gain_' (tag <> "poscGn") 0.4 $ periodicOsc_ (tag <> "posc") "smooth" (pitchf t))
                        : Nil
                    )
                )
            )
      )

lowG = (conv440 (-26)) :: Number

backgroundPlayers :: Array (Number -> List (AudioUnit D2))
backgroundPlayers =
  [ atT 0.0
      $ ( playerBackground "a"
            (\l -> [ Tuple 0.0 0.6, Tuple l (-0.2) ])
            (\l -> [ Tuple 0.0 0.0, Tuple (l / 2.0) 1.0, Tuple (l - 0.1) 0.0 ])
            (\t -> 1.0 + 0.05 * (sin (0.2 * pi * t)))
            (\t -> lowG)
        )
  , atT 3.0
      $ ( playerBackground "b"
            (\l -> [ Tuple 0.0 (-0.6), Tuple l (0.2) ])
            (\l -> [ Tuple 0.0 0.0, Tuple (l / 2.0) 1.0, Tuple (l - 0.1) 0.0 ])
            (\t -> 1.0 + 0.1 * (cos (0.2 * pi * t)))
            (\t -> lowG + 1.0 * (sin (pi * t * 0.1)))
        )
  , atT 6.0
      $ ( playerBackground "c"
            (\l -> [ Tuple 0.0 0.6, Tuple l (-0.2) ])
            (\l -> [ Tuple 0.0 0.0, Tuple (l / 2.0) 1.0, Tuple (l - 0.1) 0.0 ])
            (\t -> 1.0 + 0.05 * (sin (0.2 * pi * t)))
            (\t -> lowG + (-2.0) * (sin (pi * t * 0.2)))
        )
  , atT 9.0
      $ ( playerBackground "d"
            (\l -> [ Tuple 0.0 (-0.6), Tuple l (0.2) ])
            (\l -> [ Tuple 0.0 0.0, Tuple (l / 2.0) 1.0, Tuple (l - 0.1) 0.0 ])
            (\t -> 1.0 + 0.1 * (cos (0.2 * pi * t)))
            (\t -> lowG + 5.0 * (sin (pi * t * 0.17)))
        )
  , atT 12.0
      $ ( playerBackground "e"
            (\l -> [ Tuple 0.0 0.6, Tuple l (-0.2) ])
            (\l -> [ Tuple 0.0 0.0, Tuple (l / 2.0) 1.0, Tuple (l - 0.1) 0.0 ])
            (\t -> 1.0 + 0.05 * (sin (0.2 * pi * t)))
            (\t -> lowG)
        )
  , atT 14.0
      $ ( playerBackground "f"
            (\l -> [ Tuple 0.0 (-0.6), Tuple l (0.2) ])
            (\l -> [ Tuple 0.0 0.0, Tuple (l / 2.0) 1.0, Tuple (l - 0.1) 0.0 ])
            (\t -> 1.0 + 0.1 * (cos (0.2 * pi * t)))
            (\t -> lowG + 1.0 * (sin (pi * t * 0.1)))
        )
  , atT 17.0
      $ ( playerBackground "g"
            (\l -> [ Tuple 0.0 0.6, Tuple l (-0.2) ])
            (\l -> [ Tuple 0.0 0.0, Tuple (l / 2.0) 1.0, Tuple (l - 0.1) 0.0 ])
            (\t -> 1.0 + 0.05 * (sin (0.2 * pi * t)))
            (\t -> lowG + (-2.0) * (sin (pi * t * 0.2)))
        )
  , atT 20.0
      $ ( playerBackground "h"
            (\l -> [ Tuple 0.0 (-0.6), Tuple l (0.2) ])
            (\l -> [ Tuple 0.0 0.0, Tuple (l / 2.0) 1.0, Tuple (l - 0.1) 0.0 ])
            (\t -> 1.0 + 0.1 * (cos (0.2 * pi * t)))
            (\t -> lowG + 4.0 * (sin (pi * t * 0.4)))
        )
  ]

background :: Number -> AudioUnit D2
background time =
  ( gain_' "bgwet" 1.0
      ( graph_ "bkgraph"
          { aggregators:
              { out: Tuple (g'add_ "bkgout") (SLProxy :: SLProxy ("combine" :/ SNil))
              , combine: Tuple (g'add_ "bkgcomb") (SLProxy :: SLProxy ("gain" :/ "bkg" :/ SNil))
              , gain: Tuple (g'gain_ "bkggraphg" 0.9) (SLProxy :: SLProxy ("del" :/ SNil))
              }
          , processors:
              { del: Tuple (g'delay_ "bkggraphdel" 0.2) (SProxy :: SProxy "bkg")
              }
          , generators:
              { bkg:
                  ( gain_ "backgroundFader" 1.0
                      ( (pannerMono_ ("panhack") 0.0 (constant_ "consthack" 0.0))
                          :| fold
                              ( map (\f -> f time)
                                  backgroundPlayers
                              )
                      )
                  )
              }
          }
      )
  )

gesture0 = Tuple 0.7 0.0 :: Tuple Number Number

gesture1 = Tuple 0.7 0.4 :: Tuple Number Number

gesture2 = Tuple 0.6 0.9 :: Tuple Number Number

gesture3 = Tuple 0.6 2.0 :: Tuple Number Number

gesture4 = Tuple 0.6 3.0 :: Tuple Number Number

gesture5 = Tuple 0.7 3.7 :: Tuple Number Number

gesture6 = Tuple 0.6 5.4 :: Tuple Number Number

gesture7 = Tuple 0.5 7.0 :: Tuple Number Number

gesture8 = Tuple 0.8 8.3 :: Tuple Number Number

gesture9 = Tuple 0.8 10.0 :: Tuple Number Number

gesture10 = Tuple 0.8 11.3 :: Tuple Number Number

gesture11 = Tuple 0.8 12.8 :: Tuple Number Number

gesture12 = Tuple 0.8 14.0 :: Tuple Number Number

gesture13 = Tuple 0.7 17.4 :: Tuple Number Number

jh :: String -> Tuple Number Number -> Number -> Number -> List (AudioUnit D2)
jh tag rg pn =
  let
    len = fst rg
  in
    boundPlayer (len + 3.0)
      ( \t ->
          pure
            ( gain_ (tag <> "gainchirp") 1.0
                ( (pannerMono_ (tag <> "jhpanhack") 0.0 (constant_ (tag <> "jhconsthack") 0.0))
                    :| ( panner_
                          (tag <> "pannerchirp")
                          pn
                          ( ( gainT_' (tag <> "chirpgn") (epwf [ Tuple 0.0 1.0, Tuple (len - 0.1) 1.0, Tuple len 0.0 ] t)
                                (playBufWithOffset_ (tag <> "jharp") "solo" 4.0 (snd rg))
                            )
                          )
                      )
                    : Nil
                )
            )
      )

onsets =
  [ Tuple 1.200000 gesture2
  , Tuple 1.500000 gesture10
  , Tuple 2.300000 gesture6
  , Tuple 3.400000 gesture12
  , Tuple 3.600000 gesture9
  , Tuple 4.300000 gesture2
  , Tuple 7.600000 gesture1
  , Tuple 8.250000 gesture7
  , Tuple 8.400000 gesture6
  , Tuple 8.800000 gesture8
  , Tuple 8.900000 gesture3
  , Tuple 9.000000 gesture13
  , Tuple 9.100000 gesture1
  , Tuple 9.800000 gesture3
  , Tuple 11.000000 gesture1
  , Tuple 13.000000 gesture6
  , Tuple 16.000000 gesture13
  , Tuple 18.000000 gesture10
  , Tuple 18.300000 gesture3
  , Tuple 18.400000 gesture0
  , Tuple 18.500000 gesture3
  , Tuple 18.550000 gesture2
  , Tuple 18.600000 gesture4
  , Tuple 18.700000 gesture2
  , Tuple 18.900000 gesture11
  , Tuple 19.100000 gesture0
  , Tuple 19.500000 gesture12
  , Tuple 20.500000 gesture6
  ] ::
    Array (Tuple Number (Tuple Number Number))

birds :: Number -> AudioUnit D2
birds time =
  convolver_ "birdsConv" "verb3"
    ( gain_ "birdsMaster" 1.0
        ( zero
            :| fold
                ( map (\f -> f time)
                    ( mapWithIndex
                        ( \i (Tuple n g) ->
                            atT n
                              $ jh
                                  (show i)
                                  g
                                  ( cos (pi * sin (440.3589342432 * pi * toNumber i))
                                  )
                        )
                        onsets
                    )
                )
        )
    )

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker'
        ( background time
            + birds time
        )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers =
      makeBuffersKeepingCache
        [ Tuple "background" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/overtonez.ogg"
        , Tuple "solo" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/ryanNasalJewsHarp.ogg"
        , Tuple "verb3" "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/Impulses/matrix-reverb3.wav"
        ]
    , engineInfo = \res _ -> res engineInfo
    , periodicWaves =
      \ctx _ res rej -> do
        pw <-
          makePeriodicWave ctx
            (0.5 +> 0.25 +> -0.1 +> 0.07 +> 0.1 +> empty)
            (0.2 +> 0.1 +> 0.01 +> -0.03 +> -0.1 +> empty)
        res $ O.singleton "smooth" pw
    }
