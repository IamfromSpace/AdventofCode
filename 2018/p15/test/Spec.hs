{-# LANGUAGE ViewPatterns #-}

import Lib
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Runner (hspec)

import Control.Monad.State.Lazy (runState)
import Data.Map (keys, lookup)
import Data.Maybe (fromJust)
import Data.Monoid (Sum(Sum))
import Prelude hiding (lookup)

import Debug.Trace (trace)

example1 :: String
example1 =
    unlines
        [ "#######" -- force break
        , "#.G.E.#"
        , "#E.G.E#"
        , "#.G.E.#"
        , "#######"
        ]

simpleHorizontal :: String
simpleHorizontal =
    unlines
        [ "#######" -- force break
        , "#.....#"
        , "#.E.G.#"
        , "#.....#"
        , "#######"
        ]

simpleVertical :: String
simpleVertical =
    unlines
        [ "#######" -- force break
        , "#..E..#"
        , "#.....#"
        , "#..G..#"
        , "#######"
        ]

complexSearch :: String
complexSearch =
    unlines
        [ "###########" -- force break
        , "#.......#.#"
        , "#..###..#.#"
        , "#.G###E.#.#"
        , "#..###..#G#"
        , "#.......#.#"
        , "###########"
        ]

deadEndSearch :: String
deadEndSearch =
    unlines
        [ "#######" -- force break
        , "#..#..#"
        , "#.E#G.#"
        , "#..#..#"
        , "#######"
        ]

dontEarlyPrune :: String
dontEarlyPrune =
    unlines
        [ "#######" -- force break
        , "#..G..#"
        , "#...G.#"
        , "#.#G#G#"
        , "#...#E#"
        , "#.....#"
        , "#######"
        ]

dontEarlyPrune2 :: String
dontEarlyPrune2 =
    unlines
        [ "#############"
        , "#..........G#"
        , "#.###########"
        , "#.#GGGGGGGGG#"
        , "#.#G#######G#"
        , "#.#G#.....#G#"
        , "#.#G#...E.#G#"
        , "#.#G#.....#G#"
        , "#.#G#.....#G#"
        , "#.#G#.....#G#"
        , "#.#G####.##G#"
        , "#........#GG#"
        , "#############"
        ]

example3 =
    unlines
        [ "#######" -- force break
        , "#.E...#"
        , "#.....#"
        , "#...G.#"
        , "#######"
        ]

example4 :: String
example4 =
    unlines
        [ "#########"
        , "#G..G..G#"
        , "#.......#"
        , "#.......#"
        , "#G..E..G#"
        , "#.......#"
        , "#.......#"
        , "#G..G..G#"
        , "#########"
        ]

example4Result :: String
example4Result =
    unlines
        [ "#########"
        , "#.......#"
        , "#..GGG..#"
        , "#..GEG..#"
        , "#G..G...#"
        , "#......G#"
        , "#.......#"
        , "#.......#"
        , "#########"
        ]

example5 :: String
example5 =
    unlines
        [ "#######" -- forcebreak
        , "#.G...#"
        , "#...EG#"
        , "#.#.#G#"
        , "#..G#E#"
        , "#.....#"
        , "#######"
        ]

example5Result :: String
example5Result =
    unlines
        [ "#######" -- forcebreak
        , "#G....#"
        , "#.G...#"
        , "#.#.#G#"
        , "#...#.#"
        , "#....G#"
        , "#######"
        ]

example6 :: String
example6 =
    unlines
        [ "#######" -- forcebreak
        , "#G..#E#"
        , "#E#E.E#"
        , "#G.##.#"
        , "#...#E#"
        , "#...E.#"
        , "#######"
        ]

example7 :: String
example7 =
    unlines
        [ "#######" -- forcebreak
        , "#E..EG#"
        , "#.#G.E#"
        , "#E.##E#"
        , "#G..#.#"
        , "#..E#.#"
        , "#######"
        ]

example8 :: String
example8 =
    unlines
        [ "#######" -- forcebreak
        , "#E.G#.#"
        , "#.#G..#"
        , "#G.#.G#"
        , "#G..#.#"
        , "#...E.#"
        , "#######"
        ]

example9 :: String
example9 =
    unlines
        [ "#######" -- forcebreak
        , "#.E...#"
        , "#.#..G#"
        , "#.###.#"
        , "#E#G#G#"
        , "#...#G#"
        , "#######"
        ]

example10 :: String
example10 =
    unlines
        [ "#########" -- forcebreak
        , "#G......#"
        , "#.E.#...#"
        , "#..##..G#"
        , "#...##..#"
        , "#...#...#"
        , "#.G...G.#"
        , "#.....G.#"
        , "#########"
        ]

nextPointToFight :: Unit a -> Vec -> String -> Maybe Vec
nextPointToFight unit p (parse1 -> elfMap) =
    let goalPs = findInRangeOfPoints elfMap $ findPointsOfType unit elfMap
    in getNextStep elfMap goalPs p

main :: IO ()
main =
    hspec $ do
        describe "correct turn ordering" $ do
            it "should order example 1 correctly" $
                (getTurnOrder $
                 (\(a, _, _) -> a) $ identify () $ parse1 example1) `shouldBe`
                [Goblin 0, Elf 0, Elf 1, Goblin 1, Elf 2, Goblin 2, Elf 3]
            it "should get the point order correct too" $
                let (elfMap, unitMap, _) = identify () $ parse1 example1
                    turnOrder = getTurnOrder elfMap
                    pointOrder =
                        fmap
                            (\t ->
                                 (\(Sum y, Sum x) -> (y, x)) $
                                 fst $ fromJust $ lookup t unitMap)
                            turnOrder
                in pointOrder `shouldBe`
                   [(1, 2), (1, 4), (2, 1), (2, 3), (2, 5), (3, 2), (3, 4)]
        describe "selects the correct next step" $ do
            it "works for the elf in example 2" $
                nextPointToFight (Goblin ()) (Sum 1, Sum 2) example3 `shouldBe`
                Just (Sum 0, Sum 1)
            it "works for the goblin in example 2" $
                nextPointToFight (Elf ()) (Sum 3, Sum 4) example3 `shouldBe`
                Just (Sum (-1), Sum 0)
            it "takes a correct step in a simple example to the right" $
                nextPointToFight (Goblin ()) (Sum 2, Sum 2) simpleHorizontal `shouldBe`
                Just (Sum 0, Sum 1)
            it "takes a correct step in a simple example to the left" $
                nextPointToFight (Elf ()) (Sum 2, Sum 4) simpleHorizontal `shouldBe`
                Just (Sum 0, Sum (-1))
            it "takes a correct step in a simple example up" $
                nextPointToFight (Elf ()) (Sum 3, Sum 3) simpleVertical `shouldBe`
                Just (Sum (-1), Sum 0)
            it "takes a correct step in a simple example to the left" $
                nextPointToFight (Goblin ()) (Sum 1, Sum 3) simpleVertical `shouldBe`
                Just (Sum 1, Sum 0)
            it "should handle a complex searh with many tiebreakers" $
                nextPointToFight (Goblin ()) (Sum 3, Sum 6) complexSearch `shouldBe`
                Just (Sum (-1), Sum 0)
            it "should handle a case where there is no path to any goals" $
                nextPointToFight (Goblin ()) (Sum 2, Sum 2) deadEndSearch `shouldBe`
                Nothing
            it
                "should handle a it has to go the long way through allies and walls" $
                nextPointToFight (Elf ()) (Sum 1, Sum 3) dontEarlyPrune `shouldBe`
                Just (Sum 0, Sum (-1))
            it "should handle a scenario that would want to prune early" $
                nextPointToFight (Goblin ()) (Sum 6, Sum 8) dontEarlyPrune2 `shouldBe`
                Just (Sum 1, Sum 0)
        describe "movement only game" $
            it "works for example 4" $
            let simplerMoveOnly = snd <$> runState moveOnlyGame
                nRounds = (!! 30) . iterate simplerMoveOnly
                (elfMap, _, _) = nRounds $ identify () $ parse1 example4
                result = fmap (fmap (const ())) elfMap
            in result `shouldBe` parse1 example4Result
        describe "attacking game" $ do
            it "works for example 5 map result" $
                let (elfMap, _, _) = battle $ identify 200 $ parse1 example5
                    result = fmap (fmap (const ())) elfMap
                in result `shouldBe` parse1 example5Result
            it "works for example 5 hp counts" $
                let (_, unitMap, _) = battle $ identify 200 $ parse1 example5
                    result = countHealth unitMap
                in result `shouldBe` (0, 590)
            it "example5 all together part 1" $
                (answer1 $ parse1 example5) `shouldBe` 27730
            it "example6 all together part 1" $
                (answer1 $ parse1 example6) `shouldBe` 36334
            it "example7 all together part 1" $
                (answer1 $ parse1 example7) `shouldBe` 39514
            it "example8 all together part 1" $
                (answer1 $ parse1 example8) `shouldBe` 27755
            it "example9 all together part 1" $
                (answer1 $ parse1 example9) `shouldBe` 28944
            it "example10 all together part 1" $
                (answer1 $ parse1 example10) `shouldBe` 18740
