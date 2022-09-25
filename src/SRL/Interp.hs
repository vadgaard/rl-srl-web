module SRL.Interp (module SRL.Interp, module SRL.AST, module Common.Error) where

import SRL.AST

import Common.Error
import Common.Interp

import Control.Monad.Reader
import Control.Monad.Loops (whileM_)

-- ==================
-- Running the program
-- ==================

runAST :: SRLProgram -> (Either Error VarTab, Log)
runAST (SRLProgram ttab ast) = let (vt,ms) = (execVarState vtab . interp) ast in (vt, Log vtab ms)
  where vtab = buildVTab ttab


-- ============
-- Interpreting
-- ============

interp :: Block -> VarState ()
interp (Step s) = logStep s

interp (If t b1 b2 a) = do
  q <- checkCond t

  interp $ if q then b1 else b2

  r <- checkCond a

  unless (q == r) $ logAssertErr a q r

interp (Loop a b1 b2 t) = do
  qo <- checkCond a
  unless qo $ logAssertErr a True False

  interp b1

  whileM_ (not <$> checkCond t) $ do
    interp b2

    qi <- checkCond a
    when qi $ logAssertErr a False True

    interp b1

interp (Seq b1 b2) = interp b1 >> interp b2

-- helper
logAssertErr a q r = logError (getExpPos a) (AssertionFailed a q r)
