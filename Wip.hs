module Fasta where

import Control.Applicative
import qualified Data.Map as Map
import Data.List (sortOn)

type IndexMap a = Map.Map a [Int]
type Dotplot a = Map.Map a [(Int, Int)]
type DiagonalScore = Map.Map Int DiagonalInfo

data DiagonalInfo = DiagonalInfo
    { infoScore :: Int
    , infoStart :: (Int, Int) -- ^ index into s1, s2
    , infoStop :: (Int, Int) -- ^ index into s1, s2
    } deriving (Show)

scoreThreshold :: Int
scoreThreshold = 20

sliding :: Int -> [a] -> [[a]]
sliding n xxs
    | length xxs < n = []
    | (_:xs) <- xxs = take n xxs : sliding n xs

indexMap :: (Ord c) => Int -> [c] -> IndexMap [c]
indexMap k =
    foldr (\(i, s) -> Map.insertWith (++) s [i]) Map.empty . zip [0..] . sliding k

dotplot :: (Ord c) => Int -> [c] -> [c] -> Dotplot [c]
dotplot k s1 s2 = Map.intersectionWith (liftA2 (,)) (indexMap k s1) (indexMap k s2)

diagonalScores :: Int -> Int -> Int -> Dotplot [c] -> DiagonalScore
diagonalScores k n m =
    relevant 10 .
    Map.filter ((>= scoreThreshold) . infoScore) . toScore . concat . Map.elems
  where
    relevant n = Map.fromList . take n . sortOn (infoScore . snd) . Map.toList
    toScore :: [(Int, Int)] -> DiagonalScore
    toScore =
        foldr
            (\pair@(x1, x2) ->
                  Map.insertWith
                      combine
                      (diagIndex n pair)
                      (DiagonalInfo 1 (x1, x2) (x1 + k - 1, x2 + k - 1)))
            Map.empty
      where
        diagIndex n (i, j) = j - i + (n - 1)
        DiagonalInfo s1 p1 q1 `combine` DiagonalInfo s2 p2 q2 =
            DiagonalInfo (s1 + s2) (start p1 p2) (stop q1 q2)
          where
            start (x1, y1) (x2, y2) = (min x1 x2, min y1 y2)
            stop (x1, y1) (x2, y2) = (max x1 x2, max y1 y2)

s2 = "CAGATCGTCTCGAT"
s1 = "ATCGTATCG"
