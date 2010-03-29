module HudsonScanner where
import Data.List (mapAccumL)
type SourcePos = (Int, Int)
data Token = Token {tag :: Tag, name :: String, pos :: SourcePos}

data Tag = Ident | Number | Symbol | Junk

-- | Pair each character with its source position.
prelex :: String -> [(Char, SourcePos)]
prelex xs = snd $ mapAccumL step (0,0) xs
    where
      step (r,c) x | x == '\n' = ((r+1, 0), charInfo)
                   | otherwise = ((r, c+1), charInfo)
          where charInfo = (x, (r,c))




