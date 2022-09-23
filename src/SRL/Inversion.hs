module SRL.Inversion where

import SRL.AST

import Common.Inversion

invert :: AST -> AST
invert (Step s)         = Step $ invertStep s
invert (If t b1 b2 a)   = If a (invert b1) (invert b2) t
invert (Loop a b1 b2 t) = Loop t (invert b1) (invert b2) a
invert (Seq b1 b2)      = Seq (invert b2) (invert b1)
