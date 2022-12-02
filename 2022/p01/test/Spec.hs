import Test.Hspec (parallel)
import Test.Hspec.Runner (hspec)

main :: IO ()
main = hspec $
  parallel $
    describe "pure components" $ do
      it "should work" $ 'a' `shouldBe` 'a'
