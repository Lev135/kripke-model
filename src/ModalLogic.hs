{-# LANGUAGE StandaloneDeriving #-}
module ModalLogic (
    Op(..), MLFormula(..),
    opFunc, (*&), (*|), (*->),
    Parser, pFormula
) where

import Text.Megaparsec (Parsec)
import Control.Monad.Combinators.Expr

import Data.Void (Void)
import Data.Text(Text)
import Text.Megaparsec.Char (char, space, string, space1)
import Data.Functor (($>))
import Control.Applicative (Alternative((<|>), many))
import Data.Foldable (asum)

data Op = And | Or | Impl
    deriving Eq

opFunc :: Op -> Bool -> Bool -> Bool
opFunc And  = (&&)
opFunc Or   = (||)
opFunc Impl = \a b -> not a || b

(*&) :: MLFormula p r -> MLFormula p r -> MLFormula p r
(*&) = BinOp And

(*|) :: MLFormula p r -> MLFormula p r -> MLFormula p r
(*|) = BinOp Or

(*->) :: MLFormula p r -> MLFormula p r -> MLFormula p r
(*->) = BinOp Impl

data MLFormula p r
    = Prop  p
    | Const Bool
    | BinOp Op (MLFormula p r) (MLFormula p r)
    | Not      (MLFormula p r)
    | Box   r  (MLFormula p r)
    | Dmd   r  (MLFormula p r)

deriving instance (Eq p, Eq r) => Eq (MLFormula p r)

instance Show Op where
    show And  = " /\\ "
    show Or   = " \\/ "
    show Impl = " -> "

instance (Show p, Show r) => Show (MLFormula p r) where
    show (Prop  p)          = show p
    show (Const True)       = "1"
    show (Const False)      = "0"
    show (BinOp op f f')    = "("  <> show f <> show op <> show f' <> ")"
    show (Not      f)       = "~(" <> show f <> ")"
    show (Box   r  f)       = "["  <> show r <> "]" <> show f
    show (Dmd   r  f)       = "<"  <> show r <> ">" <> show f

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme p = p <* many space1

pFormula :: Parser p -> Parser r -> Parser (MLFormula p r)
pFormula pProp pRel = makeExprParser (lexeme $ pTerm pProp pRel) (operators pRel)

pTerm :: Parser p -> Parser r -> Parser (MLFormula p r)
pTerm pProp pRel
    =   char '(' *> pFormula pProp pRel <* char ')'
    <|> Prop        <$> pProp
    <|> Const True  <$ char '1'
    <|> Const False <$ char '0'
    <|> Not         <$> (lexeme (char '~') *> pTerm pProp pRel)
    <|> Box         <$> lexeme (char '[' *> pRel <* char ']') <*> pTerm pProp pRel
    <|> Dmd         <$> lexeme (char '<' *> pRel <* char '>') <*> pTerm pProp pRel

operators :: Parser r -> [[Operator Parser (MLFormula p r)]]
operators pRel = [
        -- [ Prefix $ Not <$  lexeme (char '~')
        -- , Prefix $ Box <$> lexeme (char '[' *> pRel <* char ']')
        -- , Prefix $ Dmd <$> lexeme (char '<' *> pRel <* char '>')
        -- ],
        [ InfixL $ BinOp And  <$ lexeme (string "/\\")],
        [ InfixL $ BinOp Or   <$ lexeme (string "\\/")],
        [ InfixR $ BinOp Impl <$ lexeme (string "->")]
    ]
