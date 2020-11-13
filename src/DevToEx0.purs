module E101.Cuica where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, loopBuf, runInBrowser)
import Type.Klank.Dev (Klank, klank, makeBuffersKeepingCache)

scene :: Number -> Behavior (AudioUnit D2)
scene time = pure (loopBuf "cuica" 1.0 0.0 0.0)

main :: Klank
main =
  klank
    { run =
      runInBrowser scene
    , buffers =
      makeBuffersKeepingCache
        [ Tuple "cuica" "https://klank-share.s3-eu-west-1.amazonaws.com/e101/test/ryanCuica.ogg"
        ]
    }
