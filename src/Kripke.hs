module Kripke (
    Model (..),
    eval, evalAll
) where
import ModalLogic (Op(..), MLFormula(..), opFunc)

data Model w r p = Model {
        rels   :: r -> w -> w -> Bool,
        worlds :: [w],
        propEv :: p -> w -> Bool
    }

eval :: Model w r p -> MLFormula p r -> w -> Bool
eval model@Model{rels, propEv, worlds} f w = case f of
    Prop  p       -> propEv p w
    Const b       -> b
    BinOp op f f' -> opFunc op (ev f w) (ev f' w)
    Not      f    -> not (ev f w)
    Box   r  f    -> all (\w' -> not (rels r w w') || ev f w') worlds
    Dmd   r  f    -> any (\w' -> rels r w w' && ev f w') worlds
    where
        ev = eval model

evalAll :: Model w r p -> MLFormula p r -> [Bool]
evalAll model@Model{worlds} f = eval model f <$> worlds
