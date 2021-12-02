input :: Read a => IO [a]
input = map read . lines <$> getContents

zipDropWith :: (a -> a -> b) -> Int -> [a] -> [b]
zipDropWith f n = zipWith f <*> drop n

countIncreases :: Ord a => Int -> [a] -> Int
countIncreases n = sum . map fromEnum . zipDropWith (<) n

main :: IO ()
main = do depths <- input :: IO [Int]
          print . countIncreases 1 $ depths
          print . countIncreases 3 $ depths
