data Instruction = Forward Int
                 | Down Int
                 | Up Int

instance Read Instruction where
  readsPrec _ s = [(Forward n, u) | ("forward", t) <- lex s, (n, u) <- reads t]
               ++ [(Down n,    u) | ("down",    t) <- lex s, (n, u) <- reads t]
               ++ [(Up n,      u) | ("up",      t) <- lex s, (n, u) <- reads t]

input :: Read a => IO [a]
input = map read . lines <$> getContents

follow :: (Int, Int) -> Instruction -> (Int, Int)
follow (x,y) (Forward n) = (x+n, y)
follow (x,y) (Down n)    = (x, y+n)
follow (x,y) (Up n)      = (x, y-n)

followAim :: (Int, Int, Int) -> Instruction -> (Int, Int, Int)
followAim (x,y,z) (Forward n) = (x+n, y+n*z, z)
followAim (x,y,z) (Down n)    = (x,   y,     z+n)
followAim (x,y,z) (Up n)      = (x,   y,     z-n)

dropAim :: (Int, Int, Int) -> (Int, Int)
dropAim (x,y,_) = (x,y)

main :: IO ()
main = do instructions <- input :: IO [Instruction]
          print . uncurry (*) . foldl follow (0,0) $ instructions
          print . uncurry (*) . dropAim . foldl followAim (0,0,0) $ instructions
