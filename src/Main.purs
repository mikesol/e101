module Klank.Dev where

import Prelude
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, gain', runInBrowser, sinOsc, speaker')
import Type.Klank.Dev (Klank, klank)

scene :: Number -> Behavior (AudioUnit D1)
scene _ = pure (speaker' (gain' 0.2 (sinOsc 440.0)))

main :: Klank
main =
  klank
    { run = runInBrowser scene
    }
