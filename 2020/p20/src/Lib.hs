module Lib where

import AdventOfCode.Util (Vector(..), multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Arrow
       ((&&&), (***), (<+>), (<<<), (>>>), (|||), arr)
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import qualified Data.Char as Char
import qualified Data.Either as Either
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum(..))
import Data.Sequence (Seq(..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Prelude hiding ((++), init, lookup, map)
import Text.ParserCombinators.PArrow (MD, (>>!))
import qualified Text.ParserCombinators.PArrow as PA
import Text.Read (readMaybe)

parseTileInfo :: String -> Int
parseTileInfo s =
    case Split.splitOneOf " :" s of
        (_:n:t) -> read n

-- Like a Unit circle : E/N/W/S, where we parse both L to R for both top and
-- bottom and then top to bottom for both left and right, this allows us to
-- more easily see if thingsg line kp
-- 
-- The Tile also stores its (unbordered) image data, but it does so only on odd
-- integers, so that the grid can be rotated more easily around the origin.
type Tile = ((String, String, String, String), Set (Vector (Integer, Integer)))

toValue :: String -> Int
toValue =
    List.foldl' (\x acc -> acc * 2 + x) 0 .
    fmap
        (\c ->
             if c == '#'
                 then 1
                 else 0)

--parseGrid ::
--       (Integral a, Num a) => ((a, a) -> Char -> b -> b) -> b -> String -> b
parseTile :: [String] -> (Int, Tile)
parseTile s =
    let (tile:grid) = s
        trans = List.transpose grid
        imgGridSize = length (head grid) - 2
        imgGrid =
            take imgGridSize $ drop 1 $ fmap (take imgGridSize . drop 1) grid
        gridParser (x, y) c s =
            if c == '#'
                then Set.insert
                         (Vector
                              ( x * 2 - (fromIntegral imgGridSize) + 1
                              , y * 2 - (fromIntegral imgGridSize) + 1))
                         s
                else s
    in ( parseTileInfo tile
       , ( (last trans, head grid, head trans, last grid)
         , Util.parseGrid gridParser mempty (unlines imgGrid)))

parse1 :: String -> [(Int, Tile)]
parse1 = fmap parseTile . multiLines

parse2 :: String -> _
parse2 = parse1

left :: Tile -> Tile
left ((e, n, w, s), img) =
    ((reverse s, e, reverse n, w), Set.map (Util.rotate Util.left) img)

right :: Tile -> Tile
right ((e, n, w, s), img) =
    ( (n, reverse w, s, reverse e)
    , Set.map (Util.rotate (Util.left <> Util.left <> Util.left)) img)

around :: Tile -> Tile
around ((e, n, w, s), img) =
    ( (reverse w, reverse s, reverse e, reverse n)
    , Set.map (Util.rotate (Util.left <> Util.left)) img)

mirrorYV :: Vector (Integer, Integer) -> Vector (Integer, Integer)
mirrorYV (Vector (x, y)) = Vector (-x, y)

mirrorY :: Tile -> Tile
mirrorY ((e, n, w, s), img) =
    ((w, List.reverse n, e, List.reverse s), Set.map mirrorYV img)

mirrorX :: Tile -> Tile
mirrorX = around . mirrorY

east :: Tile -> String
east ((e, _, _, _), _) = e

north :: Tile -> String
north ((_, n, _, _), _) = n

west :: Tile -> String
west ((_, _, w, _), _) = w

south :: Tile -> String
south ((_, _, _, s), _) = s

combos :: Tile -> [Tile]
combos =
    sequence
        [ id
        , left
        , around
        , right
        , mirrorY
        , left . mirrorY
        , around . mirrorY
        , right . mirrorY
        ]

southSideCandidatesAll :: Map Int [Tile] -> Tile -> [(Int, Tile)]
southSideCandidatesAll ts (south -> s) =
    filter ((==) s . north . snd) $ concatMap sequence $ Map.toList ts

northSideCandidatesAll :: Map Int [Tile] -> Tile -> [(Int, Tile)]
northSideCandidatesAll ts (north -> n) =
    filter ((==) n . south . snd) $ concatMap sequence $ Map.toList ts

westSideCandidatesAll :: Map Int [Tile] -> Tile -> [(Int, Tile)]
westSideCandidatesAll ts (west -> w) =
    filter ((==) w . east . snd) $ concatMap sequence $ Map.toList ts

eastSideCandidatesAll :: Map Int [Tile] -> Tile -> [(Int, Tile)]
eastSideCandidatesAll ts (east -> e) =
    filter ((==) e . west . snd) $ concatMap sequence $ Map.toList ts

pointOptions ::
       (Int, Int)
    -> (Map Int [Tile], Map (Int, Int) (Int, Tile))
    -> [(Int, Tile)]
pointOptions (x, y) (unknown, known) =
    Set.toList $
    foldl1 Set.intersection $
    Maybe.catMaybes
        [ (Set.fromList . southSideCandidatesAll unknown . snd) <$>
          Map.lookup (x, y + 1) known
        , (Set.fromList . northSideCandidatesAll unknown . snd) <$>
          Map.lookup (x, y - 1) known
        , (Set.fromList . westSideCandidatesAll unknown . snd) <$>
          Map.lookup (x + 1, y) known
        , (Set.fromList . eastSideCandidatesAll unknown . snd) <$>
          Map.lookup (x - 1, y) known
        ]

solvePoint' ::
       (Int, Int)
    -> Map Int [Tile]
    -> Map (Int, Int) (Int, Tile)
    -> Maybe (Int, Tile)
solvePoint' p unknown known =
    case pointOptions p (unknown, known) of
        [x] -> Just x
        _ -> Nothing

accept :: _ -> _ -> _
accept (p, (id, t)) (unknown, known) =
    (Map.delete id unknown, Map.insert p (id, t) known)

solvePoint ::
       (Int, Int)
    -> (Map Int [Tile], Map (Int, Int) (Int, Tile))
    -> Maybe (Map Int [Tile], Map (Int, Int) (Int, Tile))
solvePoint p (unknown, known) =
    (\t -> accept (p, t) (unknown, known)) <$> solvePoint' p unknown known

getPoi :: Map (Int, Int) a -> [(Int, Int)]
getPoi m =
    filter (\p -> not (Map.member p m)) $
    Set.toList $
    Set.fromList $
    concatMap (\(x, y) -> [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]) $
    Map.keys m

step ::
       (Map Int [Tile], Map (Int, Int) (Int, Tile))
    -> (Bool, (Map Int [Tile], Map (Int, Int) (Int, Tile)))
step state@(_, known) =
    let poi = getPoi known
        solved = take 1 $ Maybe.catMaybes $ fmap (flip solvePoint state) poi
    in case solved of
           [x] -> (False, x)
           _ -> (True, state)

fill ::
       (Map Int [Tile], Map (Int, Int) (Int, Tile))
    -> (Map Int [Tile], Map (Int, Int) (Int, Tile))
fill state =
    let (done, state') = step state
    in if done
           then state'
           else fill state'

getGuesses ::
       (Map Int [Tile], Map (Int, Int) (Int, Tile))
    -> [((Int, Int), (Int, Tile))]
getGuesses state@(_, known) =
    let poi = getPoi known
    in concatMap (\(p, os) -> fmap ((,) p) os) $
       filter ((\len -> len > 1) . length . snd) $
       fmap (\p -> (p, pointOptions p state)) poi

solve ::
       (Map Int [Tile], Map (Int, Int) (Int, Tile))
    -> Maybe (Map (Int, Int) (Int, Tile))
solve state =
    let (unknown, known) = fill state
    in if Map.size unknown == 0
           then Just known
           else let guesses = getGuesses (unknown, known)
                in if length guesses == 0
                       then Nothing
                       else case Maybe.catMaybes $
                                 fmap
                                     (solve . flip accept (unknown, known))
                                     guesses of
                                (x:_) -> Just x
                                _ -> Nothing

hasPair :: Set Tile -> String -> Bool
hasPair ts s =
    let rs = reverse s
    in any
           (\t ->
                s == east t ||
                rs == east t ||
                s == north t ||
                rs == north t ||
                s == west t || rs == west t || s == south t || rs == south t)
           ts

unpairedEdges :: Set Tile -> Tile -> Int
unpairedEdges ts t =
    length $ filter not $ fmap (hasPair ts) [east t, north t, west t, south t]

findCorners :: [(Int, Tile)] -> [(Int, Tile)]
findCorners ts =
    let asSet = Set.fromList $ fmap snd ts
    in filter (\(_, t) -> unpairedEdges (Set.delete t asSet) t >= 2) ts

findEdges :: [(Int, Tile)] -> [(Int, Tile)]
findEdges ts =
    let asSet = Set.fromList $ fmap snd ts
    in filter (\(_, t) -> unpairedEdges (Set.delete t asSet) t == 1) ts

answer1 :: [(Int, Tile)] -> _
answer1 = product . fmap fst . findCorners

unpairedEdgeIds :: Set Tile -> Tile -> [Int]
unpairedEdgeIds ts t =
    fmap fst $
    filter (not . snd) $
    zip [0 ..] $
    fmap (hasPair (Set.delete t ts)) [east t, north t, west t, south t]

-- We place each tile so that its unpaired edge faces East
orientedEdgeTiles :: [(Int, Tile)] -> [(Int, Tile)]
orientedEdgeTiles ts =
    let tileSet = Set.fromList $ fmap snd ts
        edgeTilesWithEdge =
            filter ((==) 1 . length . fst) $
            zip (fmap (unpairedEdgeIds tileSet . snd) ts) ts
        ori ([0], (id, t)) = (id, t)
        ori ([1], (id, t)) = (id, right t)
        ori ([2], (id, t)) = (id, around t)
        ori ([3], (id, t)) = (id, left t)
    in fmap ori edgeTilesWithEdge

-- We place each tile so that its unpaired edge faces East and North
orientedCornerTiles :: [(Int, Tile)] -> [(Int, Tile)]
orientedCornerTiles ts =
    let tileSet = Set.fromList $ fmap snd ts
        cornerTilesWithEdge =
            filter ((==) 2 . length . fst) $
            zip (fmap (unpairedEdgeIds tileSet . snd) ts) ts
        ori ([0, 1], (id, t)) = (id, t)
        ori ([0, 3], (id, t)) = (id, left t)
        ori ([1, 2], (id, t)) = (id, right t)
        ori ([2, 3], (id, t)) = (id, around t)
    in fmap ori cornerTilesWithEdge

-- given a bottom edge, give back the oriented edge tiles that can place top there
findEdgesFromOriented :: Map Int Tile -> String -> [(Int, Tile)]
findEdgesFromOriented orientedEdges e =
    Maybe.catMaybes $
    fmap
        (\(id, t) ->
             if e == north t
                 then Just (id, t)
                 else if e == south t
                          then Just (id, mirrorX t)
                          else Nothing)
        (Map.toList orientedEdges)

solveEdgesSouthOf :: Map Int Tile -> (Int, Tile) -> [(Int, Tile)]
solveEdgesSouthOf orientedEdges (t@(id, x)) =
    let oe = Map.delete id orientedEdges
    in t :
       case findEdgesFromOriented oe (south x) of
           [next] -> solveEdgesSouthOf oe next
           _ -> []

solvePerimeter :: [(Int, Tile)] -> Map Int Tile -> Map (Int, Int) (Int, Tile)
solvePerimeter (h:orientedCorners) orientedEdges =
    let mirrorCorners =
            concatMap (\(id, c) -> [(id, c), (id, right (mirrorY c))])
        cornersWithNorthEdge e = filter (\(_, c) -> north (right c) == e)
        --
        eastSide = solveEdgesSouthOf orientedEdges h
        --
        finalEastEdge = south $ snd $ last eastSide
        [seCorner] =
            cornersWithNorthEdge finalEastEdge $ mirrorCorners orientedCorners
        orientedCorners' = filter ((/=) (fst seCorner) . fst) orientedCorners
        southSide = solveEdgesSouthOf orientedEdges seCorner
        --
        finalSouthEdge = south $ snd $ last southSide
        [swCorner] =
            cornersWithNorthEdge finalSouthEdge $ mirrorCorners orientedCorners'
        orientedCorners'' = filter ((/=) (fst swCorner) . fst) orientedCorners'
        westSide = solveEdgesSouthOf orientedEdges swCorner
        --
        finalWestEdge = south $ snd $ last westSide
        [nwCorner] =
            cornersWithNorthEdge finalWestEdge $ mirrorCorners orientedCorners''
        northSide = solveEdgesSouthOf orientedEdges nwCorner
        --
        height = length eastSide + 1
        width = length northSide + 1
        --
        eastPoints =
            zip (fmap ((,) (width - 1)) [height - 1,height - 2 ..]) eastSide
        southPoints =
            zip (fmap (flip (,) 0) [width - 1,width - 2 ..]) $
            fmap (fmap right) southSide
        westPoints = zip (fmap ((,) 0) [0 ..]) $ fmap (fmap around) westSide
        northPoints =
            zip (fmap (flip (,) (height - 1)) [0 ..]) $
            fmap (fmap left) northSide
    in Map.fromList (eastPoints <> southPoints <> westPoints <> northPoints)

answer2 :: _ -> _
answer2 ts
    --Util.prettyPrintPointSet '#' '.' $
    --Set.fromList $
    --Map.keys $
    --let i = 1
    --in solve
    --       ( fmap combos $ Map.fromList (take i t <> drop (i + 1) t)
    --       , Map.fromList [((0, 0), t !! i)])
 =
    let corners = orientedCornerTiles ts
        edges = Map.fromList $ orientedEdgeTiles ts
        solvedPerimeter = solvePerimeter corners edges
        middlePieces =
            fmap combos $
            List.foldl'
                (flip Map.delete)
                (Map.fromList ts)
                (Map.keys edges <> fmap fst corners)
    in fmap fst $ snd $ fill (middlePieces, solvedPerimeter)

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: _
ex1_1 = undefined

ex1_2 :: _
ex1_2 = undefined

ex1_3 :: _
ex1_3 = undefined

ex1_4 :: _
ex1_4 = undefined

ex1_5 :: _
ex1_5 = undefined

ex2_1 :: _
ex2_1 = undefined

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
