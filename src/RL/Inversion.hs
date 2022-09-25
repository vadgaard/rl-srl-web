module RL.Inversion
( invert
) where

import RL.AST

import Common.Inversion ( invertStep )

invert :: RLProgram -> RLProgram
invert (RLProgram ttab ast) = RLProgram ttab $ reverse . map (\(l,b) -> (l, invertBlock b)) $ ast

invertBlock :: Block -> Block
invertBlock (f,s,j) = (invertJump j, invertSteps s, invertFrom f)

invertJump :: Jump -> From
invertJump (Goto l p)     = From l p
invertJump (If e l1 l2 p) = Fi e l1 l2 p
invertJump (Exit p)       = Entry p

invertFrom :: From -> Jump
invertFrom (From l p)     = Goto l p
invertFrom (Fi e l1 l2 p) = If e l1 l2 p
invertFrom (Entry p)      = Exit p

invertSteps :: [Step] -> [Step]
invertSteps = reverse . map invertStep
