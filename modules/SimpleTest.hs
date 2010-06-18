-- Testing Framework
-- Stephen Mann
-- October 24, 2009


module SimpleTest 
  ( AssertResult
  , assertEq
  , assertT
  , assertF
  , runAsserts
  ) where


-- define types and helpers
-- ========================

type AssertName = String
type FailMsg    = String

data AssertResult 
  = Pass AssertName
  | Fail AssertName FailMsg

instance Show AssertResult where
  show (Pass name)         = "  pass (" ++ name ++ ")"
  show (Fail name failMsg) = "* FAIL (" ++ name ++ ") " ++ failMsg

isPass, isFail :: AssertResult -> Bool
isPass (Pass _) = True
isPass _        = False
isFail          = not . isPass


-- define assertions
-- =================

assertEq :: (Eq a, Show a) => AssertName -> a -> a -> AssertResult
assertEq name expected actual
  | expected == actual = Pass name
  | otherwise          = Fail name failMsg
  where failMsg = show actual ++ " is not " ++ show expected

assertT, assertF :: AssertName -> Bool -> AssertResult
assertT name True  = Pass name
assertT name False = Fail name "false"
assertF name       = assertT name . not


-- display results
-- ===============

runAsserts :: [AssertResult] -> String
runAsserts asserts = intro ++ list ++ totals 
  where 
    intro  = "Running " ++ show (length asserts) ++ " asserts:\n"
    list   = unlines . addNumbering $ map show asserts
    totals = success ++ ": failed "  ++ show fails
                     ++ " + passed " ++ show passes
                     ++ " = total "  ++ show total
      where
        fails   = length $ filter isFail asserts
        passes  = length $ filter isPass asserts
        total   = length asserts
        success = if passes == total then "SUCCESS" else "FAILURE"

addNumbering :: [String] -> [String]
addNumbering msgs = zipWith addPadding prefixes msgs
  where 
    prefixes = map (\n -> show n ++ ". ") [1..]
    maxPrefixLen = maximum . map length $ take (length msgs) prefixes
    addPadding p m = p ++ spaces ++ m
      where spaces = concat $ replicate (maxPrefixLen - length p) " "


-- examples
-- ========

examples :: [AssertResult]
examples = 
  [ assertEq "inequality" 4 4
  , assertEq "inequality, to fail" 4 5
  , assertT "bool" True
  , assertT "bool, to fail" False
  ]
