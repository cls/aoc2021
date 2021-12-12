import Control.Monad (foldM)
import Data.Char (isLower)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set

input :: Read a => IO [a]
input = map read . lines <$> getContents

newtype Cave = Cave { name :: String }
  deriving (Eq, Ord)

instance Read Cave where
  readsPrec _ s = [(Cave x, t) | (x, t) <- lex s]

startCave :: Cave
startCave = Cave "start"

endCave :: Cave
endCave = Cave "end"

isSmall :: Cave -> Bool
isSmall = isLower . head . name

data Passage = Passage Cave Cave

instance Read Passage where
  readsPrec _ s = [(Passage a b, u) | (a, '-':t) <- reads s, (b, u) <- reads t]

fromPassage :: Passage -> [(Cave, Cave)]
fromPassage (Passage a b) | a == startCave = [(a, b)]
                          | b == endCave   = [(a, b)]
                          | b == startCave = [(b, a)]
                          | a == endCave   = [(b, a)]
                          | otherwise      = [(a, b), (b, a)]

groupSort :: Ord k => [(k, v)] -> [(k, [v])]
groupSort = map (\(xs) -> (fst (head xs), map snd xs)) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

findPaths :: ([Cave] -> Cave -> Bool) -> Map Cave [Cave] -> [[Cave]]
findPaths visit graph = map reverse $ find [] startCave
  where
    find path cave | isSmall cave && not (visit path cave) = []
                   | Just nexts <- Map.lookup cave graph   = concatMap (find (cave:path)) nexts
                   | otherwise                             = [cave:path]

visitOnce :: [Cave] -> Cave -> Bool
visitOnce path cave = not $ cave `elem` path

visitOneTwice :: [Cave] -> Cave -> Bool
visitOneTwice path cave = visitOnce path cave || notVisitedAnyTwice (filter isSmall path)

notVisitedAnyTwice :: [Cave] -> Bool
notVisitedAnyTwice = isJust . foldM uniqueVisits Set.empty
  where
    uniqueVisits xs x | Set.member x xs = Nothing
                      | otherwise       = Just (Set.insert x xs)

main :: IO ()
main = do input <- input :: IO [Passage]
          let graph = Map.fromList . groupSort . concatMap fromPassage $ input
          print . length . findPaths visitOnce     $ graph
          print . length . findPaths visitOneTwice $ graph
