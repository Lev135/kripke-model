module Models.GridJModel (
    Relation (..), Var (..),
    model, pForm, selectedWorldsPretty
) where

import Prelude hiding (getLine, putStr, putStrLn)

import ModalLogic
import Kripke

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO
import Text.Megaparsec.Char (string, space1)
import Control.Applicative (Alternative((<|>), many))
import Control.Monad (forM_)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.List (find)

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

model :: [Int] -> Model (Int, Int) Relation Var
model layerSizes = Model {
        rels,  worlds, propEv
    }
    where
        rels R0 (l, _) (l', _)  = l > l'
        rels R1 (l, w) (l', w') = l == l' && w > w'
        rels R  (l, w) (l', w') = l > l' && w' /= 0 || l == l' && w > w'

        worlds = [(l, r) | l <- [0 .. (length layerSizes - 1)], r <- [0 .. (layerSizes !! l) - 1]]
        propEv = (==)

selectedWorldsPretty :: [Int] -> [(Int, Int)] -> Text
selectedWorldsPretty layerSizes ws = T.pack <$> unlines $ reverse $ zipWith procLine [0..] layerSizes
    where
        procLine i sz = (const ' '<$> [sz .. maxSz - 1]) <> (reverse $ h i <$> [0 .. sz - 1])
        maxSz = maximum layerSizes
        h i j = case find (== (i, j)) ws of
                    Just _  -> '*' 
                    Nothing -> '_'
