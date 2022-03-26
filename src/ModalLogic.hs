module ModalLogic (
    Op(..), MLFormula(..),
    (*&), (*|), (*->)
) where

data Op = And | Or | Impl

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

instance Show Op where
    show And  = "/\\"
    show Or   = "\\/"
    show Impl = "->"

instance (Show p, Show r) => Show (MLFormula p r) where
    show (Prop  p)          = show p
    show (Const True)       = "1"
    show (Const False)      = "0"
    show (BinOp op f f')    = "("  <> show f <> show op <> show f' <> ")"
    show (Not      f)       = "~(" <> show f <> ")"
    show (Box   r  f)       = "["  <> show r <> "]" <> show f
    show (Dmd   r  f)       = "<"  <> show r <> ">" <> show f
