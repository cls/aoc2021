import Data.Function (on)
import Data.List (elemIndex, groupBy, sort, sortBy, (\\))
import Data.Maybe (fromJust, mapMaybe)

input :: Read a => IO [a]
input = map read . lines <$> getContents

data Entry = Entry { patterns :: [String], values :: [String] }

instance Read Entry where
  readsPrec _ s = let (lhs, '|':rhs) = break (== '|') s
                  in [(Entry (words lhs) (words rhs), "")]

easyDigit :: [a] -> Maybe Int
easyDigit xs = case length xs of
                 2 -> Just 1
                 3 -> Just 7
                 4 -> Just 4
                 7 -> Just 8
                 _ -> Nothing

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = groupBy ((==) `on` f) . sortBy (compare `on` f)

sortDigits :: Eq a => [[a]] -> [[a]]
sortDigits xs = let [[x1], [x7], [x4], xs235, xs069, [x8]] = groupSortOn length xs
                    [[x3], xs25] = groupSortOn (length . (\\ x1)) xs235
                    [[x5], [x2]] = groupSortOn (length . (\\ x4)) xs25
                    [xs09, [x6]] = groupSortOn (length . (\\ x1)) xs069
                    [[x9], [x0]] = groupSortOn (length . (\\ x4)) xs09
                in [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9]

entryDigits :: Entry -> Maybe [Int]
entryDigits = mapM . flip elemIndex . sortDigits . map sort . patterns <*> map sort . values

digitsValue :: [Int] -> Int
digitsValue = foldl (\xs x -> xs*10 + x) 0

main :: IO ()
main = do entries <- input :: IO [Entry]
          print . length . mapMaybe easyDigit . concatMap values $ entries
          print . sum . map digitsValue . fromJust . mapM entryDigits $ entries
