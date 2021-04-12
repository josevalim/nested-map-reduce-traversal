module StateT where

import Control.Monad.Trans.State
import Control.Monad

data Section a = Section
  { title    :: String
  , reset    :: Bool
  , lessons  :: [(String, a)]
  , position :: a
} deriving Show

sample :: [Section ()]
sample =
  [ Section
      { title = "Getting started"
      , reset = False
      , lessons = [("Welcome", ()), ("Installation", ())]
      , position = ()
      }
  , Section
      { title = "Basic operator"
      , reset = False
      , lessons = [("Addition / Subtraction", ()), ("Multiplication / Division", ())]
      , position = ()
      }
  , Section
      { title = "Advanced topics"
      , reset = True
      , lessons = [("Mutability", ()), ("Immutability", ())]
      , position = ()
      }
  ]

--------------------------------------------------------------------------------

annotate :: [Section ()] -> [Section Int]
annotate sections = flip evalState 1 $ sequence
  [ do
      when (reset section) (put 1)

      newLessons <- sequence
        [ do
            j <- inc
            pure (name, j)
        | (name, _) <- lessons section
        ]

      pure $ section
        { position = i
        , lessons = newLessons
        }
  | (i, section) <- zip [1..] sections
  ]
  where
    inc = state $ \i -> (i, i + 1)

main :: IO ()
main = print (annotate sample)
