{-# LANGUAGE NamedFieldPuns #-}

import Data.Foldable (foldl')

data Chapter = MkChapter { ctitle :: String
                         , cresetLessonPosition :: Bool
                         , clessons :: [Lesson]
                         }
               deriving (Eq, Show)

data Lesson = MkLesson { lname :: String
                       }
            deriving (Eq, Show)

data PosChapter = MkPosChapter { pctitle :: String
                               , pcresetLessonPosition :: Bool
                               , pcposition :: Int
                               , pclessons :: [PosLesson]
                               }
                deriving (Eq, Show)

data PosLesson = MkPosLesson { plname :: String
                             , plposition :: Int
                             }
               deriving (Eq, Show)

solve :: [Chapter] -> [PosChapter]
solve = ($ []) . trd . foldl' go (1, 1, id)
  where
    trd (_, _, x) = x
    go :: (Int, Int, [PosChapter] -> [PosChapter]) -> Chapter -> (Int, Int, [PosChapter] -> [PosChapter])
    go (nc, nl, acc) MkChapter{ctitle, cresetLessonPosition, clessons} =
      let pchapter = MkPosChapter { pctitle = ctitle
                                  , pcresetLessonPosition = cresetLessonPosition
                                  , pcposition = nc
                                  , pclessons = plessons
                                  }
          nl' = if cresetLessonPosition then 1 else nl
          plessons = [ MkPosLesson {plname = lname, plposition = m}
                     | (m, MkLesson{lname}) <- zip [nl'..] clessons
                     ]
      in (nc + 1, nl' + length plessons, acc . (pchapter:))

input :: [Chapter]
input = [ MkChapter { ctitle = "Getting started"
                    , cresetLessonPosition = False
                    , clessons = [ MkLesson { lname = "Welcome" }
                                 , MkLesson { lname = "Installation" }
                                 ]
                    }
        , MkChapter { ctitle = "Basic operator"
                    , cresetLessonPosition = False
                    , clessons = [ MkLesson { lname = "Addition / Subtraction" }
                                 , MkLesson { lname = "Multiplication / Division" }
                                 ]
                    }
        , MkChapter { ctitle = "Advanced topics"
                    , cresetLessonPosition = True
                    , clessons = [ MkLesson { lname = "Mutability" }
                                 , MkLesson { lname = "Immutability" }
                                 ]
                    }
        ]


expected :: [PosChapter]
expected = [ MkPosChapter { pctitle = "Getting started"
                          , pcresetLessonPosition = False
                          , pcposition = 1
                          , pclessons = [ MkPosLesson {plname = "Welcome", plposition = 1}
                                        , MkPosLesson {plname = "Installation", plposition = 2}
                                        ]
                          }
           , MkPosChapter { pctitle = "Basic operator"
                          , pcresetLessonPosition = False
                          , pcposition = 2
                          , pclessons = [ MkPosLesson {plname = "Addition / Subtraction", plposition = 3}
                                        , MkPosLesson {plname = "Multiplication / Division", plposition = 4}
                                        ]
                          }
           , MkPosChapter { pctitle = "Advanced topics"
                          , pcresetLessonPosition = True
                          , pcposition = 3
                          , pclessons = [ MkPosLesson {plname = "Mutability", plposition = 1}
                                        , MkPosLesson {plname = "Immutability", plposition = 2}
                                        ]
                          }
           ]
