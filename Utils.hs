module Utils where
import Data.Foldable

argMin :: Ord a => [a] -> (Int, a)
argMin l = minimumBy comp $ zip [0..] l
  where comp a b = compare (snd a) (snd b)

argMax :: Ord a => [a] -> (Int, a)
argMax l = maximumBy comp $ zip [0..] l
  where comp a b = compare (snd a) (snd b)
