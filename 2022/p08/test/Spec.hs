import Aoc (ScanMode (..), parse, scanT, sumT)
import Clash.Prelude (IO, Index, Maybe (..), Vec, const, flip, fmap, fromIntegral, id, snd, toList, ($), (.), (<>), (==))
import Control.Monad (join)
import Data.Char (ord)
import qualified Data.List as List
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.Hspec.Runner (hspec)
import Util (MoreOrDone (..), adapt, pureSim, pureSimWithRam)

main :: IO ()
main =
  hspec $
    describe "p08" $ do
      describe "pure simulation of (sumT . scanT)" $
        modifyMaxSuccess (const 10000) $
          prop
            "spatial transforms preserve visibility count"
            transforms
      describe "pure example" $
        it "should return 21" $
          flip shouldBe [Just 21] $
            List.take 1 $
              List.dropWhile ((==) Nothing) $
                fmap (join . snd) $
                  pureSim (adapt sumT) 0 $
                    fmap (\(_, _, b) -> b) $
                      pureSimWithRam scanT ((0 :: Index 5, 0), Receiving) $
                        fmap
                          parse
                          ( fmap (fromIntegral . ord) "30373\n25512\n65332\n33549\n35390\n"
                              <> [4]
                              <> List.repeat 10
                          )

transforms :: Vec 5 (Vec 5 (Index 10)) -> _
transforms xs =
  let f0 = id
      f1 = List.transpose . f0
      f2 = List.reverse . f1
      f3 = List.transpose . f2
      f4 = List.reverse . f3
      f5 = List.transpose . f4
      f6 = List.reverse . f5
      f7 = List.transpose . f6
      -- All possible transformations that yield the same visiblity (I think)
      fs = [f0, f1, f2, f3, f4, f5, f6, f7]
      go f =
        List.take 1 $
          List.dropWhile ((==) Nothing) $
            fmap (join . snd) $
              pureSim (adapt sumT) 0 $
                fmap (\(_, _, b) -> b) $
                  pureSimWithRam
                    scanT
                    ((0 :: Index 5, 0), Receiving)
                    ( fmap (Just . More) (List.concat $ f $ fmap toList $ toList xs)
                        <> [Just Done]
                        <> List.replicate 10000 Nothing
                    )
      answers = fmap go fs
   in answers `shouldBe` List.replicate (List.length fs) (List.head answers)
