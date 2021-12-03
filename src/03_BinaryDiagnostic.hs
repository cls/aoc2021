mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

input :: Read a => IO [a]
input = map read . lines <$> getContents

type Bits = [Bool]

newtype Binary = Binary { getBits :: Bits }

readsBits :: ReadS Bits
readsBits ('0':t) = mapFst (False :) <$> readsBits t
readsBits ('1':t) = mapFst (True  :) <$> readsBits t
readsBits s       = [([], s)]

instance Read Binary where
  readsPrec _ = map (mapFst Binary) . readsBits

frequency :: [Bits] -> [Int]
frequency = foldr1 (zipWith (+)) . map (map fromEnum)

mostCommon :: [Bits] -> Bits
mostCommon xs = map (\x -> x*2 >= length xs) $ frequency xs

leastCommon :: [Bits] -> Bits
leastCommon = map not . mostCommon

fromBits :: Bits -> Int
fromBits = foldl (\xs x -> xs*2 + fromEnum x) 0

gammaRate :: [Bits] -> Int
gammaRate = fromBits . mostCommon

epsilonRate :: [Bits] -> Int
epsilonRate = fromBits . leastCommon

powerConsumption :: [Bits] -> Int
powerConsumption xs = gammaRate xs * epsilonRate xs

-- This function is ugly but I don't have time to improve it.
filterByBits :: ([Bits] -> Bits) -> [Bits] -> Bits
filterByBits f = filterBits 0
  where
    filterBits _ [x] = x
    filterBits k xs  = filterBits (k+1) $ filter (\x -> x !! k == f xs !! k) xs

generatorRating :: [Bits] -> Int
generatorRating = fromBits . filterByBits mostCommon

scrubberRating :: [Bits] -> Int
scrubberRating = fromBits . filterByBits leastCommon

lifeSupportRating :: [Bits] -> Int
lifeSupportRating xs = generatorRating xs * scrubberRating xs

main :: IO ()
main = do binary <- input :: IO [Binary]
          let bits = map getBits binary
          print $ powerConsumption bits
          print $ lifeSupportRating bits
