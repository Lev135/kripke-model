module Main where

import ModalLogic

data Relation = R1 | R2
data Var = A | B | C

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

main :: IO ()
main = do
    print trans1A
