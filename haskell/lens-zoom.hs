#!/usr/bin/env stack
{- stack --resolver lts-17.5 script --package lens --package aeson
 --package raw-strings-qq --package lens-aeson --package mtl --package bytestring
 --package generic-lens -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Lens
import Control.Monad.State
import Data.Aeson (Value (..), decode, encode)
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Generics.Labels
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Text.RawString.QQ

data Counters = Counters {lessonCnt :: Int, groupCnt :: Int}
  deriving (Generic)

type St s a = StateT s (State Counters) a

solution :: St Value ()
solution =
  _Array . each . _Object %> do
    shouldReset <- gets (^?! ix "reset_lesson_position" . _Bool)
    when shouldReset $ lift (#lessonCnt .= 1)

    topIdx <- lift (#groupCnt <<+= 1)
    at "position" ?= Number (fromIntegral topIdx)

    ix "lessons" . _Array . each . _Object %> do
      idx <- lift (#lessonCnt <<+= 1)
      at "position" ?= Number (fromIntegral idx)
  where
    infixl 8 %>
    (%>) l act = zoom l act

runSolution :: Value -> Value
runSolution v = evalState (execStateT solution v) (Counters 1 1)

input :: Value
input =
  fromJust . decode $
    [r|
    [
      {
        "title": "Getting started",
        "reset_lesson_position": false,
        "lessons": [ {"name": "Welcome"}, {"name": "Installation"} ]
      },
      {
        "title": "Basic operator",
        "reset_lesson_position": false,
        "lessons": [ {"name": "Addition / Subtraction"}, {"name": "Multiplication / Division"} ]
      },
      {
        "title": "Advanced topics",
        "reset_lesson_position": true,
        "lessons": [ {"name": "Mutability"}, {"name": "Immutability"} ]
      }
    ]

    |]

main :: IO ()
main = do
  let result = runSolution input
  C8.putStrLn $ encode result
