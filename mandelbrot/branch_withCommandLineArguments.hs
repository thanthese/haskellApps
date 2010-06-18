-- Mandelbrot Set Explorer
-- Stephen Mann
-- Mon Dec 28 13:52:09 CST 2009

import Data.Complex
import Data.List ( isPrefixOf )
import System( getArgs )


-- TODO:
-- - command-line arguments
-- - better coloring
-- - threading?


-- user-defined constants
-- ======================

maxIterationDepth :: Int
maxIterationDepth = 100

--width, height :: Int
--width  = 20
--height = width

upperLeft, lowerRight :: (Double, Double)
upperLeft  = (-2.0, -2.0)
lowerRight = ( 2.0,  2.0)


-- axes
-- ====

{-
xAxis, yAxis :: [Double]
xAxis = makeRange fst width
yAxis = makeRange snd height
-}

makeRange :: ((Double, Double) -> Double) -> Int -> [Double]
makeRange f m = [n1, nPlus..n2]
  where
    n1    = f upperLeft
    n2    = f lowerRight
    nPlus = n1 + (n2 - n1) / (fromIntegral m - 1)


-- Image
-- =====

type Color = Int
type Image = String

colorMaxValue :: Color
colorMaxValue
  | maxIterationDepth > 255 = 255
  | otherwise               = maxIterationDepth

resultMapToImage :: ResultMap -> Image
resultMapToImage = (imageHeader ++) . unlines . map convertRow
  where
    convertRow  = unwords . map (show . testResultToColor)
    imageHeader = "P2"                    ++ "\n"
      ++ "# By Stephen Mann"              ++ "\n"
      ++ show width ++ " " ++ show height ++ "\n"
      ++ show colorMaxValue               ++ "\n"

main :: IO ()
main = do
  args <- getArgs
  print args
  print $ getArg 'w' args "100"
  let width  = read (getArg 'w' args "100") :: Int
  let height = read (getArg 'h' args "100") :: Int


  let xAxis = makeRange fst width
  let yAxis = makeRange snd height

  print width
  putStrLn $ resultMapToImage resultMap

getArg :: Char -> [String] -> String -> String
getArg flag args optDefault 
  | null matchesList = optDefault
  | otherwise        = drop 2 $ head matchesList
  where matchesList = filter (['-', flag] `isPrefixOf`) args


-- TestResult
-- ==========

type ResultMap = [[TestResult]]

data TestResult = InSet | EscapedAt Int
  deriving (Show, Eq)

testResultToColor :: TestResult -> Color
testResultToColor InSet         = 0
testResultToColor (EscapedAt n) = n `mod` colorMaxValue


-- program core
-- ============

resultMap :: ResultMap
resultMap = map generateRow yAxis
  where generateRow row = map (testPoint . (:+ row)) xAxis

testPoint :: Complex Double -> TestResult
testPoint c = testPoint' (0 :+ 0) 0
  where
    testPoint' z i
      | hasEscaped z           = EscapedAt i
      | i >= maxIterationDepth = InSet
      | otherwise              = testPoint' iterated (i + 1)
      where iterated = z*z + c
    hasEscaped (a :+ b) = a*a + b*b > 4
