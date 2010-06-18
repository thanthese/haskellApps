{-
  TODO
  ====

  - foldr needs to be changed to a foldl'
-}

import Data.List
import Data.Complex

main = putStr $ createImage "432" 300

initialBBox       = (-2,2,2,-2)
colorMaxValue     = 5
maxIterationDepth = 100

type BBox = (Double, Double, Double, Double)

createImage :: String -> Int -> String
createImage zoomStr width = imageHeader ++ unlines (map makeRow yAxis)
  where
    makeRow y = unwords $ map (show . flip calcPoint y) xAxis

    (x1, y1, x2, y2) = calcFinalBBox zoomStr
    xAxis = makeAxis x1 x2
    yAxis = makeAxis y1 y2

    makeAxis a b = [a, a + ((b - a) / fromIntegral (width - 1)) .. b]

    imageHeader = "P2"                   ++ "\n"
      ++ "# By Stephen Mann"             ++ "\n"
      ++ show width ++ " " ++ show width ++ "\n"
      ++ show colorMaxValue              ++ "\n"

calcFinalBBox :: String -> BBox
calcFinalBBox = foldr ($) initialBBox . map zoom
  where
    zoom n (x1, y1, x2, y2) = case n of
      '1' -> ( xAvg , y1   , x2   , yAvg )
      '2' -> ( x1   , y1   , xAvg , yAvg )
      '3' -> ( x1   , yAvg , xAvg , y2   )
      '4' -> ( xAvg , yAvg , x2   , y2   )
      where
        xAvg = (x1 + x2) / 2
        yAvg = (y1 + y2) / 2

calcPoint :: Double -> Double -> Int
calcPoint x y = calcPoint' (0 :+ 0) 0
  where
    c = x :+ y
    calcPoint' z i
      | i >= maxIterationDepth = 0
      | hasEscaped z           = i `mod` colorMaxValue
      | otherwise              = calcPoint' (iteratePt z) (i + 1)
      where iteratePt z = z*z + c
    hasEscaped (a :+ b) = a*a + b*b > 400

{-
(Char -> String -> String) -> String -> [Char] -> String
foldr (:) "" "abcdefg"
:t foldr

foldr (\c s -> "(" ++ show c ++ " " ++ s ++ ")" ) "" "abcdefg"

(String -> Char -> String) -> String -> [Char] -> String
foldl' (\s c -> "(" ++ show c ++ " " ++ s ++ ")" ) "A" "abcdefg"
:t foldl
-}
