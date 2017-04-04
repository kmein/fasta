{-# LANGUAGE TemplateHaskell #-}

module Fasta where

import Control.Applicative
import Control.Lens
       ((^.), (.~), (%~), _1, _2, both, makeLenses, over)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Traversable as Trav

k = 1

_SCORE_THRESHOLD = 0

_GAP_THRESHOLD = 4

_HIT_SCORE = 5

_MISS_SCORE = -4

_GAP_START_PENALTY = -5

_GAP_EXTENSION_PENALTY = -100

gapDeltas :: [Int]
gapDeltas = take _GAP_THRESHOLD [-_GAP_THRESHOLD ..] ++ take _GAP_THRESHOLD [1 ..]

type IndexMap = Map.Map String [Int]

type Dotplot = Map.Map String [(Int, Int)]

data DiagonalInfo = DiagonalInfo
    { _dIndex :: Int
    , _dScore :: Int
    , _dStart :: (Int, Int)
    , _dStop :: (Int, Int)
    } deriving (Show)

makeLenses ''DiagonalInfo

instance Eq DiagonalInfo where
    d1 == d2 = d1 ^. dScore == d2 ^. dScore

instance Ord DiagonalInfo where
    d1 <= d2 = d1 ^. dScore <= d2 ^. dScore

type DiagonalScores = Map.Map Int DiagonalInfo

data Region
    = SingleDiagonal DiagonalInfo
    | DoubleDiagonal DiagonalInfo
                     DiagonalInfo
    deriving (Show)

instance Eq Region where
    r1 == r2 = regionScore r1 == regionScore r2

instance Ord Region where
    r1 <= r2 = regionScore r1 <= regionScore r2

type RegionScores = Map.Map Int Region

regionScore :: Region -> Int
regionScore (SingleDiagonal di) = di ^. dScore
regionScore (DoubleDiagonal di1 di2) =
    di1 ^. dScore + di2 ^. dScore + gapPenalty gaps
  where
    gaps = gapSize di1 di2

gapPenalty :: Int -> Int
gapPenalty gaps = _GAP_START_PENALTY + _GAP_EXTENSION_PENALTY * (gaps - 1)

gapSize :: DiagonalInfo -> DiagonalInfo -> Int
gapSize di1 di2 =
    abs $ di1 ^. dIndex - di2 ^. dIndex

initIndexMap :: IndexMap
initIndexMap = Map.empty

adjustWithDefault
    :: Ord k
    => a -> (a -> a) -> k -> Map.Map k a -> Map.Map k a
adjustWithDefault def f key m =
    if key `Map.member` m
        then Map.adjust f key m
        else Map.insert key def m

indexMap :: String -> IndexMap
indexMap =
    foldr (\(i, s) -> Map.insertWith (++) s [i]) Map.empty .
    zip [0 ..] . sliding k
  where
    sliding n xxs
        | length xxs < n = []
        | (_:xs) <- xxs = take n xxs : sliding n xs

dotplot :: String -> String -> Dotplot
dotplot s1 s2 = Map.intersectionWith (liftA2 (,)) (indexMap s1) (indexMap s2)

diagIndex :: Int -> (Int, Int) -> Int
diagIndex n (i, j) = j - i + (n - 1)

diagonalScores :: Int -> Dotplot -> DiagonalScores
diagonalScores n dp = diagonalScores' dp
  where
    diagonalScores' :: Dotplot -> DiagonalScores
    diagonalScores' =
        takeN 10 .
        Map.filter ((>= _SCORE_THRESHOLD) . (^. dScore)) .
        toDiagScore . concat . Map.elems

    takeN :: (Ord k, Ord v) => Int -> Map.Map k v -> Map.Map k v
    takeN n' = Map.fromList . take n' . sort . Map.toList

    toDiagScore :: [(Int, Int)] -> DiagonalScores
    toDiagScore =
        foldr
            (\pair@(s1, s2) m ->
                 let adjustStart1 = (dStart . _1) %~ min s1
                     adjustStart2 = (dStart . _2) %~ min s2
                     adjustStart = adjustStart1 . adjustStart2
                     adjustStop1 = (dStop . _1) %~ max (s1 + k - 1)
                     adjustStop2 = (dStop . _2) %~ max (s2 + k - 1)
                     adjustStop = adjustStop1 . adjustStop2
                     adjustScore = dScore %~ (+ 1)
                     diag_index = diagIndex n pair
                 in adjustWithDefault
                        (DiagonalInfo diag_index 1 pair (over both (+ (k - 1)) pair)) -- if insert
                        (adjustScore . adjustStart . adjustStop) -- if adjust
                        diag_index -- key
                        m)
            Map.empty

scoreStrings :: String -> String -> Int
scoreStrings [] [] = 0
scoreStrings (x:xs) (y:ys) = scoreChars x y + scoreStrings xs ys
  where
    scoreChars :: Char -> Char -> Int
    scoreChars c1 c2
        | c1 == c2 = _HIT_SCORE
        | otherwise = _MISS_SCORE
scoreStrings _ _ = error "scoreStrings expects strings of the same length"

diagInfoSubstrings :: String -> String -> DiagonalInfo -> (String, String)
diagInfoSubstrings s1 s2 diag_info =
    let drop_s1 = drop $ diag_info ^. (dStart . _1)
        drop_s2 = drop $ diag_info ^. (dStart . _2)
        take_s1 =
            take $ diag_info ^. (dStop . _1) - diag_info ^. (dStart . _1) + 1
        take_s2 =
            take $ diag_info ^. (dStop . _2) - diag_info ^. (dStart . _2) + 1
    in ((take_s1 . drop_s1) s1, (take_s2 . drop_s2) s2)

rescoreDiagonals :: String -> String -> DiagonalScores -> DiagonalScores
rescoreDiagonals s1 s2 =
    Map.map
        (\diag_index ->
             let (s1', s2') = diagInfoSubstrings s1 s2 diag_index
             in dScore .~ scoreStrings s1' s2' $ diag_index)

calculateRegions :: DiagonalScores -> RegionScores
calculateRegions ds = Map.map (diagonalInfoToRegion ds) ds

diagonalInfoToRegion :: DiagonalScores -> DiagonalInfo -> Region
diagonalInfoToRegion ds diag_info =
    let diag_indices = map (+ diag_info ^. dIndex) gapDeltas
        ds' = Map.filterWithKey (\key _ -> key `elem` diag_indices) ds
    in Map.fold
           (\diag_info' region ->
                max region $ DoubleDiagonal diag_info diag_info')
           (SingleDiagonal diag_info)
           ds'

_s1 = "ABCDE"

_s2 = "ABDEFCDE"

_dp = dotplot _s1 _s2

_ds = diagonalScores (length _s1) _dp

_ds' = rescoreDiagonals _s1 _s2 _ds
