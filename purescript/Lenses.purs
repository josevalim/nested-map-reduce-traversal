module Lenses
  ( modify
  ) where

import Prelude
import Data.Foldable (length)
import Data.Lens (iover)
import Data.Lens.Indexed (itraversed)
import Data.Lens.Record (prop)
import Data.Traversable (mapAccumL)
import Prim.Row (class Lacks)
import Record as Record
import Type.Prelude (Proxy(..))

type Lesson r
  = { name :: String | r }

type Section r a
  = { title :: String
    , reset_lesson_position :: Boolean
    , lessons :: Array (Lesson a)
    | r
    }

type Input
  = Section () ()

type Output
  = Section ( position :: Int ) ( position :: Int )

_position :: Proxy "position"
_position = Proxy

_lessons :: Proxy "lessons"
_lessons = Proxy

addPosition :: forall r. Lacks "position" r => Int -> Int -> { | r } -> { position :: Int | r }
addPosition offset i = Record.insert _position (offset + i)

addNestedPositions :: Int -> Input -> { accum :: Int, value :: Section () ( position :: Int ) }
addNestedPositions index section =
  let
    index' = if section.reset_lesson_position then 1 else index
  in
    { accum: index' + length section.lessons
    , value: iover (prop _lessons <<< itraversed) (addPosition index') section
    }

modify :: Array Input -> Array Output
modify =
  iover itraversed (addPosition 1)
    <<< _.value
    <<< mapAccumL addNestedPositions 1
