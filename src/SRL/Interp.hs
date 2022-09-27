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
  logInfo $ "Conditional: if " ++ show t ++ " then .. else .. fi " ++ show a
  q <- checkCond t
  logInfo $ "Condition evaluated to " ++ show q

  interp $ if q then b1 else b2

  r <- checkCond a

  if (q == r)
  then logInfo "Exit condition matches entry condition!"
  else logAssertErr a q r

interp (Loop a b1 b2 t) = do
  logInfo $ "Loop: for " ++ show a ++ " do .. loop .. until " ++ show t
  qo <- checkCond a
  if qo
  then logInfo "Condition is true on entry! Executing first loop body..."
  else logAssertErr a True False

  interp b1

  whileM_ (not <$> checkCond t) $ do
    logInfo "Exit condition not satisfied. Executing second loop body.."
    interp b2

    qi <- checkCond a
    if qi
    then logAssertErr a False True
    else logInfo "Condition is false on loop as expected!\nExecuting first loop body.."

    interp b1
  logInfo "Exit condition is satisfied!"

interp (Seq b1 b2) = interp b1 >> interp b2

-- helper
logAssertErr a q r = logError (getExpPos a) (AssertionFailed a q r)
