input :: Read a => IO [a]
input = map read . lines <$> getContents

zipTailWith :: (a -> a -> b) -> [a] -> [b]
zipTailWith f = zipWith f <*> drop 1

zipTailWith3 :: (a -> a -> a -> b) -> [a] -> [b]
zipTailWith3 f = zipWith3 f <*> drop 1 <*> drop 2

countIncreases :: Ord a => [a] -> Int
countIncreases = sum . map fromEnum . zipTailWith (<)

add3 :: Num a => a -> a -> a -> a
add3 x y z = x + y + z

main :: IO ()
main = do depths <- input :: IO [Int]
          print . countIncreases $ depths
          print . countIncreases . zipTailWith3 add3 $ depths
