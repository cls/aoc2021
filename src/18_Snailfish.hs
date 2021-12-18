import Control.Monad ((<=<))

input :: Read a => IO [a]
input = map read . lines <$> getContents

data Number = Value Int
               | Pair Number Number
  deriving (Eq)

instance Read Number where
  readsPrec _ ('[':s) = [(Pair x y, u) | (x, ',':t) <- reads s, (y, ']':u) <- reads t]
  readsPrec _ s       = [(Value n, t) | (n, t) <- reads s]

instance Show Number where
  show (Value n)  = show n
  show (Pair x y) = "[" ++ show x ++ "," ++ show y ++ "]"

magnitude :: Number -> Int
magnitude (Value n)  = n
magnitude (Pair x y) = magnitude x * 3 + magnitude y * 2

add :: Number -> Number -> Number
add x y = reduce (Pair x y)

reduce :: Number -> Number
reduce x | Just x' <- explode [] x = reduce x'
         | Just x' <- split      x = reduce x'
         | otherwise               = x

fromLeft :: Either a b -> Maybe a
fromLeft (Left  x) = Just x
fromLeft (Right _) = Nothing

fromRight :: Either a b -> Maybe b
fromRight (Right x) = Just x
fromRight (Left  _) = Nothing

explode :: [Either Number Number] -> Number -> Maybe Number
explode ctx (Value _)                  = Nothing
explode ctx (Pair (Value m) (Value n)) | length ctx >= 4 = let explodeLeft  = first (fmap Left  . rightmost (+m) <=< fromLeft)
                                                               explodeRight = first (fmap Right . leftmost  (+n) <=< fromRight)
                                                            in Just . rebuild (Value 0) . explodeLeft . explodeRight $ ctx
explode ctx (Pair x y)                 = explode (Right y : ctx) x `orElse` explode (Left x : ctx) y

split :: Number -> Maybe Number
split (Value n)  | n > 9              = Just $ Pair (Value $ n `div` 2) (Value $ (n + 1) `div` 2)
                 | otherwise          = Nothing
split (Pair x y) | Just x' <- split x = Just $ Pair x' y
                 | Just y' <- split y = Just $ Pair x y'
                 | otherwise          = Nothing

orElse :: Maybe a -> Maybe a -> Maybe a
orElse x@(Just _) _ = x
orElse _          y = y

rebuild :: Number -> [Either Number Number] -> Number
rebuild x (Left  y : ctx) = rebuild (Pair y x) ctx
rebuild x (Right y : ctx) = rebuild (Pair x y) ctx
rebuild x []              = x

first :: (a -> Maybe a) -> [a] -> [a]
first f (x:xs) | Just x' <- f x = x' : xs
               | otherwise      = x  : first f xs
first _ []     = []

leftmost :: (Int -> Int) -> Number -> Maybe Number
leftmost f (Value n)                            = Just $ Value (f n)
leftmost f (Pair x y) | Just x' <- leftmost f x = Just $ Pair x' y
leftmost f (Pair x y) | Just y' <- leftmost f y = Just $ Pair x  y'
leftmost _ _                                    = Nothing

rightmost :: (Int -> Int) -> Number -> Maybe Number
rightmost f (Value n)                             = Just $ Value (f n)
rightmost f (Pair x y) | Just y' <- rightmost f y = Just $ Pair x  y'
rightmost f (Pair x y) | Just x' <- rightmost f x = Just $ Pair x' y
rightmost _ _                                     = Nothing

pairs :: [a] -> [(a, a)]
pairs xs = [(x,y) | x <- xs, y <- xs]

main :: IO ()
main = do numbers <- input :: IO [Number]
          print . magnitude . foldl1 add $ numbers
          print . maximum . map (magnitude . uncurry add) . filter (uncurry (/=)) . pairs $ numbers
