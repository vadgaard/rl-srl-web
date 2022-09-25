{-# LANGUAGE GADTs #-}
module RL.AST (module RL.AST, module Common.AST) where

import Data.List (intercalate)

import Common.AST

-- =============
-- An RL program
-- =============

data RLProgram where
  RLProgram :: TypeTab -> AST -> RLProgram

instance Show RLProgram where
  show (RLProgram ttab ast) = showAST ttab ast


-- ===
-- AST
-- ===

-- labels
type Label = String

type AST = [(Label, Block)]
showAST :: TypeTab -> AST -> String
showAST ttab ast = showTypeTab ttab ++ (intercalate "\n\n" . map (\(l,b) -> l ++ ": " ++ showBlock b)) ast

type Block = (From, [Step], Jump)
showBlock (f,s,j) = show f ++ "\n  "
                 ++ (
                      if null s
                      then show (Skip (0,0))
                      else (intercalate "\n  " . map show) s
                    ) ++ "\n"
                 ++ show j

data From = From Label Pos
          | Fi Exp Label Label Pos
          | Entry Pos
          deriving Eq
instance Show From where
  show (From l _)     = "from " ++ l
  show (Fi e l1 l2 _) = "fi " ++ showPar e ++ " " ++ l1 ++ " " ++ l2
  show (Entry _)      = "entry"

data Jump = Goto Label Pos
          | If Exp Label Label Pos
          | Exit Pos
          deriving Eq
instance Show Jump where
  show (Goto l _)     = "goto " ++ l
  show (If e l1 l2 _) = "if "  ++ showPar e ++ " "  ++ l1 ++ " " ++ l2
  show (Exit _)       = "exit"
