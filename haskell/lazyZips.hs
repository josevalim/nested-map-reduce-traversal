import Data.Maybe
import Data.Functor

data Section a = Section
  { title      :: String
  , reset      :: Bool
  , lessons    :: [(String, a)]
  , annotation :: a
  } deriving Show

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe ls = Just $ last ls

annotateSection sectionCounter lessonCounter section =
  section
    { annotation = sectionCounter
    , lessons = zipWith ($>) (lessons section) [lessonCounter..]
    }

annotate :: [Section a] -> [Section Integer]
annotate sections = result
  where
    result = zipWith3 annotateSection [1..] lessonCounters sections
    lessonCounters = zipWith newCounter prevLastCounters (map reset sections)
    newCounter prevCounter reset = if reset then 1 else prevCounter + 1
    prevLastCounters = 0 : zipWith fromMaybe prevLastCounters lastCounters
    lastCounters = map (fmap snd . lastMaybe . lessons) result

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

empty = Section { title = "t", reset = False, lessons = [], annotation = () }
