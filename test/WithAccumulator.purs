module Test.WithAccumulator where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (IAudioUnit(..), runInBrowser, sinOsc, speaker')
import Type.Klank.Dev (Klank', affable, klank)

scene ::
  { onset :: Maybe Number } ->
  Number ->
  Behavior (IAudioUnit D1 { onset :: Maybe Number })
scene acc time =
  pure
    $ IAudioUnit
        ( speaker'
            ( sinOsc 440.0
            )
        )
        (acc { onset = Nothing })

main :: Klank' { onset :: Maybe Number }
main =
  klank
    { run = runInBrowser scene
    , accumulator = affable $ pure ({ onset: Just 1.0 })
    }
