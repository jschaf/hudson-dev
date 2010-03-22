import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))
import Control.Monad

data HDecl = HDecl HFunc
           deriving (Show)

data HFunc = HFunc {name :: HId,
                    params :: HParams,
                    code :: [HDecl]}
           deriving (Show)

type HId = String
type HObject = String
type HParams = [HId]            -- Optional types

p_func :: CharParser () HFunc
p_func = do spaces
            string "function"
            spaces
            return HFunc {name = "blah", params = ["id1"], code = []}

p_id :: CharParser () HId
p_id = do s  <- (char '.' <|> letter)
          ss <- many alphaNum
          return (s:ss)

p_params = p_series '(' p_id ')'

p_series :: Char -> CharParser () a -> Char -> CharParser () [a]
p_series left p right =
    between (char left <* spaces) (char right) $
                (p <* spaces) `sepBy` (char ',' <* spaces)
