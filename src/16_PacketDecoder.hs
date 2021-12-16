import Data.Bits (bit, testBit, (.|.))
import Data.Char (digitToInt)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

data Packet = Literal { version :: Int, value :: Int }
            | Operator { version :: Int, function :: [Int] -> Int, subpackets :: [Packet] }

type Bits = [Bool]
type ReadBits a = Bits -> (a, Bits)

hexToBits :: Char -> Bits
hexToBits = flip map [3, 2, 1, 0] . testBit . digitToInt

readPacket :: ReadBits Packet
readPacket (v1:v2:v3:t1:t2:t3:xs) = let version = bitsToInt [v1,v2,v3]
                                        typeId  = bitsToInt [t1,t2,t3]
                                     in readPacketType typeId version xs

bitsToInt :: Bits -> Int
bitsToInt = foldr (.|.) 0 . zipWith (\i x -> if x then bit i else 0) [0..] . reverse

readPacketType :: Int -> Int -> ReadBits Packet
readPacketType 0 = readOperator sum
readPacketType 1 = readOperator product
readPacketType 2 = readOperator minimum
readPacketType 3 = readOperator maximum
readPacketType 4 = readLiteral
readPacketType 5 = readBinaryOperator (>)
readPacketType 6 = readBinaryOperator (<)
readPacketType 7 = readBinaryOperator (==)

readLiteral :: Int -> ReadBits Packet
readLiteral v = mapFst (Literal v . bitsToInt) . readGroups

readGroups :: ReadBits Bits
readGroups (True :xs) = let (group,  next) = splitAt 4 xs
                            (groups, rest) = readGroups next
                         in (group ++ groups, rest)
readGroups (False:xs) = splitAt 4 xs

readOperator :: ([Int] -> Int) -> Int -> ReadBits Packet
readOperator f v = mapFst (Operator v f) . readSubpackets

readBinaryOperator :: (Int -> Int -> Bool) -> Int -> ReadBits Packet
readBinaryOperator f = readOperator (\[x, y] -> if f x y then 1 else 0)

readSubpackets :: ReadBits [Packet]
readSubpackets (False:xs) = let (lenBits, next) = splitAt 15 xs
                                (subBits, rest) = splitAt (bitsToInt lenBits) next
                                (subs,    [])   = readPackets maxBound subBits
                             in (subs, rest)
readSubpackets (True :xs) = let (numBits, next) = splitAt 11 xs
                             in readPackets (bitsToInt numBits) next

readPackets :: Int -> ReadBits [Packet]
readPackets _ [] = ([], [])
readPackets 0 xs = ([], xs)
readPackets n xs = let (sub,  next) = readPacket xs
                       (subs, rest) = readPackets (n-1) next
                    in (sub:subs, rest)

versions :: Packet -> [Int]
versions (Literal  v _)    = [v]
versions (Operator v _ xs) = v : concatMap versions xs

evaluate :: Packet -> Int
evaluate (Literal  _ x)    = x
evaluate (Operator _ f xs) = f (map evaluate xs)

main :: IO ()
main = do (packet, _) <- readPacket . concatMap hexToBits <$> getContents
          print . sum . versions $ packet
          print . evaluate $ packet
