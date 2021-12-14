import Data.Map (Map, (!))
import qualified Data.Map as Map

data Polymer = Polymer { elements :: Map Char Int, pairs :: Map (Char, Char) Int }

type Rules = Map (Char, Char) Char

input :: IO (Polymer, Rules)
input = do (templateStr:"":ruleStrs) <- lines <$> getContents
           return (polymer templateStr, rules ruleStrs)

polymer :: String -> Polymer
polymer = Polymer . counts <*> counts . (zip <*> tail)

rules :: [String] -> Rules
rules = Map.fromList . map (\[x,y,' ','-','>',' ',z] -> ((x, y), z))

counts :: Ord k => [k] -> Map k Int
counts = foldr (`add` 1) Map.empty

add :: Ord k => k -> Int -> Map k Int -> Map k Int
add = Map.insertWith (+)

polymerize :: Rules -> Polymer -> Polymer
polymerize rs (Polymer es ps) = Map.foldrWithKey poly (Polymer es Map.empty) ps
  where
    poly (x,z) n (Polymer es ps) = let y = rs ! (x,z)
                                       es' = add y n $ es
                                       ps' = add (x,y) n . add (y,z) n $ ps
                                   in Polymer es' ps'

polymerRating :: Polymer -> Int
polymerRating (Polymer es _) = maximum es - minimum es

main :: IO ()
main = do (template, rules) <- input :: IO (Polymer, Rules)
          let polymers = iterate (polymerize rules) template
          print . polymerRating $ polymers !! 10
          print . polymerRating $ polymers !! 40
