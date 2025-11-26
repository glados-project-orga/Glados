
module Main (main) where
import Test.HUnit

tests :: Test
tests = TestList[]

main :: IO Counts
main = runTestTT tests
