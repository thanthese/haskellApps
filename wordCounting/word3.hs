import Prelude as P
import Data.List as L
import Data.Char as C
import Data.ByteString.Lazy.Char8 as B

{-
  Change program so that the map on the map directly prints.
-}

main = do
  contents <- B.getContents 
  let lower = B.map (P.map C.toLower) contents
  B.putStr lower
