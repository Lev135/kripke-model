module Main where

import ModalLogic
import Data.Text (Text)
import Text.Megaparsec (parse, errorBundlePretty, MonadParsec (eof))
import Text.Megaparsec.Char (string)
import Control.Applicative (Alternative((<|>)))

data Relation = R1 | R2
    deriving Eq
data Var = A | B | C
    deriving Eq

instance Show Relation where
    show R1 = "1"
    show R2 = "2"

instance Show Var where
    show A = "A"
    show B = "B"
    show C = "C"

type Formula = MLFormula Var Relation

box1 :: Formula -> Formula
box1 = Box R1

trans1A :: Formula
trans1A = box1 (box1 $ Prop A) *-> box1 (Prop A)

pVar :: Parser Var
pVar =  A <$ string "A"
    <|> B <$ string "B"
    <|> C <$ string "C"

pRel :: Parser Relation
pRel = R1 <$ string "1" <|> R2 <$ string "2"

pForm :: Parser (MLFormula Var Relation)
pForm = pFormula pVar pRel 

parsePart :: Parser a -> Text -> Either String a
parsePart p s = case parse p "" s of
    Left  e -> Left $ errorBundlePretty e
    Right a -> return a

parseAll :: Parser a -> Text -> Either String a
parseAll p = parsePart (p <* eof)


main :: IO ()
main = do
    a <-case parseAll pForm "[1][1]A -> [1]A" of
        Left  e -> fail e
        Right a -> return a
    print $ trans1A == a
