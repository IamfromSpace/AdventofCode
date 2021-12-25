import Lib (tests)
import Test.Hspec (parallel)
import Test.Hspec.Runner (hspec)

main :: IO ()
main = hspec $ parallel tests
