module Kripke (

) where

data Frame w r = Frame {
        worlds    :: [w],
        relations :: [(r, w -> w -> Bool)]
    }
