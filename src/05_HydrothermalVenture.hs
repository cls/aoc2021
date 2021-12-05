import Data.Map (Map)
import qualified Data.Map as Map

input :: Read a => IO [a]
input = map read . lines <$> getContents

type Point = (Int, Int)

data Vector = Vector Point Point

instance Read Vector where
  readsPrec _ s = [(Vector (x1,y1) (x2,y2), w) | (x1,             ',':t) <- reads s
                                               , (y1, ' ':'-':'>':' ':u) <- reads t
                                               , (x2,             ',':v) <- reads u
                                               , (y2,                 w) <- reads v]

signumFromTo :: Int -> Int -> [Int]
signumFromTo a b = [a, a + signum (b - a) .. b]

diagonalPoints :: Vector -> [Point]
diagonalPoints (Vector (x1,y1) (x2,y2)) = signumFromTo x1 x2 `zip` signumFromTo y1 y2

orthogonalPoints :: Vector -> [Point]
orthogonalPoints v@(Vector (x1,y1) (x2,y2)) | x1 == x2 || y1 == y2 = diagonalPoints v
                                            | otherwise            = []

plotPoint :: Point -> Map Point Int -> Map Point Int
plotPoint p = Map.insertWith (+) p 1

plotPoints :: [Point] -> Map Point Int
plotPoints = foldr plotPoint Map.empty

countOverlaps :: [Point] -> Int
countOverlaps = Map.size . Map.filter (>=2) . plotPoints

main :: IO ()
main = do vectors <- input :: IO [Vector]
          print . countOverlaps . concatMap orthogonalPoints $ vectors
          print . countOverlaps . concatMap diagonalPoints   $ vectors
