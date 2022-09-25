module SRL.Inversion where

import SRL.AST

import Common.Inversion

invert :: SRLProgram -> SRLProgram
invert (SRLProgram ttab ast) = SRLProgram ttab $ invertAST ast

invertAST :: AST -> AST
invertAST (Step s)         = Step $ invertStep s
invertAST (If t b1 b2 a)   = If a (invertAST b1) (invertAST b2) t
invertAST (Loop a b1 b2 t) = Loop t (invertAST b1) (invertAST b2) a
invertAST (Seq b1 b2)      = Seq (invertAST b2) (invertAST b1)
