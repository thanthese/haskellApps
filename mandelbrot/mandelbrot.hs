-- Mandelbrot Set Explorer
-- Stephen Mann
-- Sat Jan  2 16:42:57 CST 2010

import Data.Complex

-- TODO:
-- - better palette (http://en.wikipedia.org/wiki/Mandelbrot_set#Continuous_.28smooth.29_coloring)
-- - command-line arguments? (use records)
-- - threading?
--
-- iter - logBase 2 (logBase 2 (abs coor))


-- user-defined constants
-- ======================

maxIterationDepth :: Int
maxIterationDepth = 100

width, height :: Int
width  = 300
height = width

upperLeft, lowerRight :: (Double, Double)
upperLeft  = (-2.0, -2.0)
lowerRight = ( 2.0,  2.0)

colorMaxValue :: Color
colorMaxValue = 3


-- axes
-- ====

xAxis, yAxis :: [Double]
xAxis = makeRange fst width
yAxis = makeRange snd height

makeRange :: ((Double, Double) -> Double) -> Int -> [Double]
makeRange f m = [n1, nPlus .. n2]
  where
    n1    = f upperLeft
    n2    = f lowerRight
    nPlus = n1 + (n2 - n1) / (fromIntegral m - 1)


-- Image
-- =====

type Color = Int
type Image = String

testResultToColor :: TestResult -> Color
testResultToColor InSet         = 0
testResultToColor (EscapedAt n) = n `mod` colorMaxValue

-- iter - logBase 2 (logBase 2 (abs coor))




resultMapToImage :: ResultMap -> Image
resultMapToImage = (imageHeader ++) . unlines . map convertRow
  where
    convertRow  = unwords . map (show . testResultToColor)
    imageHeader = "P2"                    ++ "\n"
      ++ "# By Stephen Mann"              ++ "\n"
      ++ show width ++ " " ++ show height ++ "\n"
      ++ show colorMaxValue               ++ "\n"

main :: IO ()
main = putStrLn $ resultMapToImage resultMap


-- TestResult
-- ==========

type ResultMap = [[TestResult]]

data TestResult = InSet | EscapedAt Int
  deriving (Show, Eq)


-- program core
-- ============

resultMap :: ResultMap
resultMap = map generateRow yAxis
  where generateRow row = map (testPoint . (:+ row)) xAxis

testPoint :: Complex Double -> TestResult
testPoint c = testPoint' (0 :+ 0) 0
  where
    testPoint' z i
      | i >= maxIterationDepth = InSet
      | hasEscaped z           = EscapedAt i
      | otherwise              = testPoint' iterated (i + 1)
      where iterated = z*z + c
    hasEscaped (a :+ b) = a*a + b*b > 400
