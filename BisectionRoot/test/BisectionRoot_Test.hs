-- BisectionRoot_Test.hs

module BisectionRoot_Test where

import qualified Distribution.TestSuite as TS
import qualified Test.HUnit as HU
import qualified BisectionRoot as BR

bisection_Test1 :: HU.Test
bisection_Test1 = HU.TestCase $ HU.assertBool
 "bisection_Test1" $
 (BR.bisectionRoot (\x -> 2*x+3) (-100,100)) == ((-3)/2)

bisection_Test2 :: HU.Test
bisection_Test2 = HU.TestCase $ HU.assertBool
 "bisection_Test2" $
 (BR.bisectionRoot (\x -> x^2 - 100) (1,200)) == 10

bisection_Test3 :: HU.Test
bisection_Test3 = HU.TestCase $ HU.assertBool
 "bisection_Test3" $
 (BR.bisectionRoot (\x -> 1/x - 1) (0.1,100)) == 1

hunitTests :: HU.Test
hunitTests = HU.TestList
 [ HU.TestLabel "bisection_Test1" bisection_Test1
 , HU.TestLabel "bisection_Test2" bisection_Test2
 , HU.TestLabel "bisection_Test3" bisection_Test3
 ]

runHUnitTests :: HU.Test -> IO TS.Progress
runHUnitTests tests = do
 (HU.Counts cases tried errors failures) <- HU.runTestTT tests
 return $ if errors > 0
  then TS.Finished $ TS.Error "There were errors in the HUnit tests"
  else if failures > 0
   then TS.Finished $ TS.Fail "There were failures in the HUnit tests"
   else TS.Finished TS.Pass

tests :: IO [TS.Test]
tests = return [TS.Test hunit] where
 hunit = TS.TestInstance
  { TS.run = runHUnitTests hunitTests
  , TS.name = "HUnit Test Cases"
  , TS.tags = ["hunit"]
  , TS.options = []
  , TS.setOption = \_ _ -> Right hunit
  }
