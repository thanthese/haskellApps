import Prelude   as P
import Data.Map  as M
import Data.Char as C
import Data.List as L

{-
  Stats
  =====

  run-time:
    version 1    : 2.3 s
    this version : 1.4 s

  x10 sample file run-time: (12M)
    version 1    : 43.0 s
    this version : 14.3 s
    python       : 3.8  s
-}

main = interact           -- IO
  $ unlines               -- convert list to string
  . P.map prettyPrintPair -- pretty print list
  . sort                  -- sort by occurrences
  . L.map flipPair        -- flips pairs in list
  . assocs                -- convert map to associative list
  . buildMap              -- build map (basically solves problem)
  . words                 -- break stream into words
  . P.map cleanChar       -- remove punctuation, to lower-case

buildMap :: [String] -> Map String Int
buildMap = L.foldl' insertWordIntoMap empty
  where insertWordIntoMap m w = insertWith (+) w 1 m

flipPair (a, b) = (b, a)

prettyPrintPair (i, s) = show i ++ ", " ++ s

cleanChar c
  | isAlpha c = toLower c
  | otherwise = ' '
