{- cabal:
build-depends: base
             , aeson
             , lens
             , lens-aeson 
             , split 
             , raw-strings-qq 
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Aeson (Value, decode, encode)
import Data.Aeson.Lens (_Array,_Object,_Integer,_Bool)
import Data.Maybe (fromJust)
import Control.Lens (over, partsOf, traversed, review, singular, ix, _2, at, set)
import Control.Lens.Unsound (lensProduct)
import Text.RawString.QQ
import Data.List.Split (split,whenElt,keepDelimsL)

--
-- Run this script with:
--      cabal run lens-parts-of.hs

-- "partsOf" is a combinators that takes a "Traversal" targeting the points in
-- the structure on which you are interested. It then gives them to you in the
-- form of a list that you can manipulate. The it takes the result list and
-- re-installs all elements in the original structure.
--
-- It's used extensively in this solution.
--
-- This solution also uses the "split" package to create groups of lessons. Each group
-- will have its own numbering starting at 1.

solve :: Value -> Value
solve = over _Array $ withLessonGroups addPositions . addPositions
  where
  addPositions :: forall f . Traversable f =>  f Value -> f Value
  addPositions = 
      over (partsOf (traversed . _Object)) $ 
          zipWith (set (at "position") . Just . review _Integer) [1..]
  withLessonGroups f = 
      over (partsOf (traversed . _Object . lensProduct (singular $ ix "reset_lesson_position" . _Bool) (singular $ ix "lessons" . _Array))) $
          \pairs -> do group <- split (keepDelimsL (whenElt fst)) pairs -- this is the list monad
                       over (partsOf (traversed . _2 . traversed)) f group
  
main :: IO ()
main = do
  let result = solve input
  putStr "result = expected : "
  print $ result == expected

input :: Value
input = fromJust . decode $ [r|
  [
    {
      "title": "Getting started",
      "reset_lesson_position": false,
      "lessons": [
        {"name": "Welcome"},
        {"name": "Installation"}
      ]
    },
    {
      "title": "Basic operator",
      "reset_lesson_position": false,
      "lessons": [
        {"name": "Addition / Subtraction"},
        {"name": "Multiplication / Division"}
      ]
    },
    {
      "title": "Advanced topics",
      "reset_lesson_position": true,
      "lessons": [
        {"name": "Mutability"},
        {"name": "Immutability"}
      ]
    }
  ]
|]

expected :: Value
expected = fromJust . decode $ [r|
  [
    {
      "title": "Getting started",
      "reset_lesson_position": false,
      "position": 1,
      "lessons": [
        {"name": "Welcome", "position": 1},
        {"name": "Installation", "position": 2}
      ]
    },
    {
      "title": "Basic operator",
      "reset_lesson_position": false,
      "position": 2,
      "lessons": [
        {"name": "Addition / Subtraction", "position": 3},
        {"name": "Multiplication / Division", "position": 4}
      ]
    },
    {
      "title": "Advanced topics",
      "reset_lesson_position": true,
      "position": 3,
      "lessons": [
        {"name": "Mutability", "position": 1},
        {"name": "Immutability", "position": 2}
      ]
    }
  ]
|]


