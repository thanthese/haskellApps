import Data.List
import Data.Char

main = interact                          -- IO
  $ unlines                              -- combine meta-data into string
  . map (\(n, w) -> show n ++ ", " ++ w) -- pretty print
  . sort                                 -- sort meta-data by occurances
  . map (\s -> (length s, head s))       -- transform to sublist meta-data
  . group                                -- break into sublists of unique words
  . sort                                 -- sort words
  . words                                -- break into words
  . map (\c ->                           -- simplify chars in input
    if isAlpha c                         
      then toLower c
      else ' ')
