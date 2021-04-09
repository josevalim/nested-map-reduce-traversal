module AccumL where

import Data.List

data Section a = Section
  { title      :: String
  , reset      :: Bool
  , lessons    :: [(String, a)]
  , annotation :: a
  } deriving Show

annotate :: [Section a] -> [Section Int]
annotate = snd . mapAccumL annotateSection (1, 1)

annotateSection :: (Int, Int) -> Section a -> ((Int, Int), Section Int)
annotateSection (sectionCounter, lessonCounter) section
  = ((sectionCounter + 1, lessonCounter' + count), section')
  where
    count          = length $ lessons section
    lessonCounter' = if reset section then 1 else lessonCounter
    section'       = section { annotation = sectionCounter, lessons = lessons' }
    lessons'       = zipWith (\(l, _) idx -> (l, idx)) (lessons section) $ enumFrom lessonCounter'

sample :: [Section ()]
sample =
  [ Section
    { title = "Getting started"
    , reset = False
    , lessons = [("Welcome", ()), ("Installation", ())]
    , annotation = ()
    }
  , Section
    { title = "Basic operator"
    , reset = False
    , lessons = [("Addition / Subtraction", ()), ("Multiplication / Division", ())]
    , annotation = ()
    }
  , Section
    { title = "Advanced topics"
    , reset = True
    , lessons = [("Mutability", ()), ("Immutability", ())]
    , annotation = ()
    }
  ]
