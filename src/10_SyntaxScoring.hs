import Data.Either (partitionEithers)
import Data.List (sort)

closeParen :: Char -> Maybe Char
closeParen '(' = Just ')'
closeParen '[' = Just ']'
closeParen '{' = Just '}'
closeParen '<' = Just '>'
closeParen _   = Nothing

syntaxCheck :: String -> Either Char String
syntaxCheck = flip check []
  where
    check (x:xs) ys     | Just y <- closeParen x = check xs (y:ys)
    check (x:xs) (y:ys) | x == y = check xs ys
    check (x:_)  _      = Left x
    check []     ys     = Right ys

scoreError :: Char -> Int
scoreError ')' = 3
scoreError ']' = 57
scoreError '}' = 1197
scoreError '>' = 25137

scoreRepair :: String -> Int
scoreRepair = foldl (\xs x -> xs * 5 + score x) 0
  where
    score ')' = 1
    score ']' = 2
    score '}' = 3
    score '>' = 4

median :: Ord a => [a] -> a
median = (!!) . sort <*> (`div` 2) . length

main :: IO ()
main = do (corrupted, incomplete) <- partitionEithers . map syntaxCheck . lines <$> getContents
          print . sum . map scoreError $ corrupted
          print . median . map scoreRepair $ incomplete
