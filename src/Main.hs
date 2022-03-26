module Main where

import Prelude hiding (getLine, putStr, putStrLn)

import ModalLogic
import Kripke

import Data.Text (Text)
import Data.Text.IO
import Text.Megaparsec (parse, errorBundlePretty, MonadParsec (eof))
import Text.Megaparsec.Char (string, space1)
import Control.Applicative (Alternative((<|>), many))
import Control.Monad (forM_)
import Text.Megaparsec.Char.Lexer (decimal)

data Relation = R1 | R0 | R
    deriving Eq

type Var = (Int, Int)

instance Show Relation where
    show R0 = "0"
    show R1 = "1"
    show R  = ""

type Formula = MLFormula Var Relation

pVar :: Parser Var
pVar =  do
    string "{"
    l <- decimal
    many space1 
    w <- decimal
    string "}"
    return (l, w)

pRel :: Parser Relation
pRel = R0 <$ string "0" <|> R1 <$ string "1" <|> return R

pForm :: Parser (MLFormula Var Relation)
pForm = pFormula pVar pRel

parsePart :: Parser a -> Text -> Either String a
parsePart p s = case parse p "" s of
    Left  e -> Left $ errorBundlePretty e
    Right a -> return a

parseAll :: Parser a -> Text -> Either String a
parseAll p = parsePart (p <* eof)

ioParseAll :: Parser a -> Text -> IO a
ioParseAll p t = case parseAll p t of
    Left  s -> fail s
    Right a -> return a

ioParseForm :: Text -> IO (MLFormula Var Relation)
ioParseForm = ioParseAll pForm

ws = [(a, b) | a <- [0 .. 5], b <- [0 .. 5]]

model :: Model (Int, Int) Relation Var
model = Model {
        rels,  worlds = ws, propEv
    }
    where
        rels R0 (l, _) (l', _)  = l > l'
        rels R1 (l, w) (l', w') = l == l' && w > w'
        rels R  (l, w) (l', w') = l > l' && w' /= 0 || l == l' && w > w'

        propEv = (==)

main :: IO ()
main = do
    forM_ [1..] $ const $ do
        fml <- getLine
        a   <- ioParseForm fml
        print $ map fst $ filter snd $ zip ws $ evalAll model a
