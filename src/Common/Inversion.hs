module Common.Inversion where

import Common.AST

invertStep :: Step -> Step
invertStep (Update id op e p) = Update id (invertOp op) e p
invertStep (Push id1 id2 p)   = Pop  id1 id2 p
invertStep (Pop  id1 id2 p)   = Push id1 id2 p
invertStep (Skip p)           = Skip p
invertStep (Swap id1 id2 p)   = Swap id1 id2 p
invertStep (Reverse id p)     = Reverse id p
invertStep (Init id exps p)   = Free id exps p
invertStep (Free id exps p)   = Init id exps p

invertOp :: UpdOp -> UpdOp
invertOp PlusEq  = MinusEq
invertOp MinusEq = PlusEq
invertOp XorEq   = XorEq
invertOp MultEq  = DivEq
invertOp DivEq   = MultEq
