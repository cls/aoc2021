import Data.List (tails, transpose)
import Data.Set (Set)
import qualified Data.Set as Set

type Beacon = (Int, Int, Int)

input :: IO [[Beacon]]
input = readScanners . lines <$> getContents

readScanners :: [String] -> [[Beacon]]
readScanners = map (map readBeacon . tail) . splitBy null

readBeacon :: String -> Beacon
readBeacon s = let [x,y,z] = splitBy (== ',') s in (read x, read y, read z)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f xs = let (ys, zs) = break f xs in ys : case zs of _:zs' -> splitBy f zs'
                                                            []    -> []

orientations :: Beacon -> [Beacon]
orientations a = let r (x,y,z) = ( x, z,-y)
                     t (x,y,z) = (-y, x, z)
                     rttt = [r, t, t, t]
                     rtr  = [r, t, r]
                     sequence = rttt ++ rttt ++ rttt ++ rtr ++ rttt ++ rttt ++ rttt
                  in scanr ($) a . reverse $ sequence

(.-.) :: Beacon -> Beacon -> Beacon
(ax,ay,az) .-. (bx,by,bz) = (ax - bx, ay - by, az - bz)

data Scanner = Scanner { offset :: Beacon, beacons :: Set Beacon }

alignments :: Set Beacon -> [Beacon] -> [Scanner]
alignments as bs = [Scanner (a .-. b') (Set.fromList (map (.-. (b' .-. a)) bs')) | bs' <- transpose (map orientations bs), a <- Set.toList as, b' <- bs']

isMatch :: Set Beacon -> Set Beacon -> Bool
isMatch as = (>= 12) . Set.size . Set.intersection as

matches :: Set Beacon -> [Beacon] -> [Scanner]
matches as bs = filter (isMatch as . beacons) $ alignments as bs

matchBeacons :: Set Beacon -> [Beacon] -> Set Beacon
matchBeacons as bs = as `Set.union` Set.unions (map beacons (matches as bs))

fixBy :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixBy p f x = let y = f x in if p x y then x else fixBy p f y

ordpairs :: [a] -> [(a, a)]
ordpairs xs = [(x, y) | x:ys <- tails xs, y <- ys]

manhattan :: Beacon -> Beacon -> Int
manhattan (ax, ay, az) (bx, by, bz) = abs (ax - bx) + abs (ay - by) + abs (az - bz)

main :: IO ()
main = do (as:bss) <- input :: IO [[Beacon]]
          let as' = fixBy (==) (\as' -> foldl matchBeacons as' bss) . Set.fromList $ as
          print . Set.size $ as'
          print . maximum . map (uncurry manhattan) . ordpairs . map (offset . head . matches as') $ bss
