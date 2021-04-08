#!/usr/bin/env stack
{-
  stack --resolver lts-17.8 script
    --package lens
    --package aeson
    --package aeson-qq
    --package lens-aeson
    --package mtl
    --package bytestring
    --package aeson-pretty
    --package raw-strings-qq
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson (Value(..), decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Maybe (fromJust)
import Control.Lens
import Control.Monad.State
import Text.RawString.QQ


solve :: [Value] -> [Value]
solve xs = flip evalState (1, 1) $ do
  forM xs $ \x -> do
    n <- use _1
    _1 += 1
    let ls = x ^?! key "lessons" . _Array
        reset = x ^?! key "reset_lesson_position" . _Bool
    when reset $ _2 .= 1
    ls' <- forM ls $ \l -> do
      m <- use _2
      _2 += 1
      pure (l & _Object . at "position" ?~ Number m)
    let x' = x
           & _Object . at "position" ?~ Number n
           & key "lessons" .~ Array ls'
    pure x'

main :: IO ()
main = do
  let result = solve input
  putStr "result = expected : "
  print $ result == expected
  C8.putStrLn $ encodePretty result

input :: [Value]
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

expected :: [Value]
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
