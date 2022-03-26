module Main where

import Prelude hiding (getLine, putStr, putStrLn)

import ModalLogic
import Kripke

import Data.Text (Text)
import Data.Text.IO
import Text.Megaparsec (parse, errorBundlePretty, MonadParsec (eof))
import Text.Megaparsec.Char (string)
import Control.Applicative (Alternative((<|>)))
import Control.Monad (forM_)

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

ioParseAll :: Parser a -> Text -> IO a
ioParseAll p t = case parseAll p t of
    Left  s -> fail s
    Right a -> return a

ioParseForm :: Text -> IO (MLFormula Var Relation)
ioParseForm = ioParseAll pForm

ws = [(a, b) | a <- [1 .. 5], b <- [1 .. 5]]

model :: Model (Int, Int) Relation Var
model = Model {
        rels,  worlds = ws, propEv
    }
    where
        rels R1 (a, _) (b, _) = a > b
        rels R2 (_, a) (_, b) = a > b

        propEv A = \(a, _) -> a == 1
        propEv B = \(_, b) -> b == 1
        propEv C = \(a, b) -> a == 1 && b == 1

main :: IO ()
main = do
    forM_ [1..] $ const $ do
        fml <- getLine
        a   <- ioParseForm fml
        print $ map fst $ filter snd $ zip ws $ evalAll model a
