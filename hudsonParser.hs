import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.Maybe

data HDecl = HDecl HFunc
           deriving (Show)

data HFunc = HFunc {name :: HId,
                    params :: HParams,
                    code :: [HDecl]}
           deriving (Show)

type HId = String
type HObject = String
type HParams = [HId]            -- Optional types

-- Not really sure why this works, but Wikipedia said it was good.
instance Applicative (GenParser tok st) where
    pure  = return
    (<*>) = ap

p_func :: CharParser () HFunc
p_func = do spaces
            string "function"
            spaces
            n <- p_id
            spaces
            ps <- p_params
            spaces
            string "is"
            spaces
            c <- p_code
            return HFunc {name = n, params = ps, code = c}

p_id :: CharParser () HId
p_id = liftM2 (:) (char '.' <|> letter) (many alphaNum)

p_code = return []

p_params = p_series '(' p_id ')'

p_series :: Char -> CharParser () a -> Char -> CharParser () [a]
p_series left p right =
    between (char left <* spaces) (char right) $
                (p <* spaces) `sepBy` (char ',' <* spaces)

ts = "function blah  (one, two) is"

p a b = parse a "hudson" b