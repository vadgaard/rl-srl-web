module RL.Interp (module RL.Interp, module RL.AST, module Common.Error) where

import RL.AST

import Common.Interp
import Common.Error

import Control.Monad.Reader

import qualified Data.HashMap.Strict as M

-- The program state
type ASTMap = M.HashMap Label Block
type ProgState = ReaderT ASTMap VarState


-- ==================
-- Running the program
-- ==================

runAST :: RLProgram -> (Either Error VarTab, Log)
runAST (RLProgram ttab ast) =
  let entry   = (fst . head) ast
      vtab    = buildVTab ttab
      (vt,ms) = execVarState vtab . runReaderT (interp [] entry) $ M.fromList ast
    in (vt, Log vtab ms)


-- ============
-- Interpreting
-- ============

interp :: Label -> Label -> ProgState ()
interp from l = do

  result <- asks (M.lookup l)
  case result of
    Just (f,ss,j) -> do
      llogInfo $ "From: " ++ show f
      case f of
        Entry p      -> unless (null from)
          $ logFromErr p (show $ Entry p) from
        From l' p    -> unless (from == l')
          $ logFromErr p from l'
        Fi a l1 l2 p -> do
          q <- lcheckCond a
          llogInfo $ "Condition evaluated to " ++ show q
          let l' = if q then l1 else l2
          if (from == l')
          then llogInfo "Labels match!"
          else logFromErr p from l'

      logSteps ss

      llogInfo $ "Jump: " ++ show j
      case j of
        Exit _       -> return ()
        Goto l' _    -> interp l l'
        If t l1 l2 p -> do
          q <- lcheckCond t
          llogInfo $ "Condition evaluated to " ++ show q
          if q then interp l l1 else interp l l2
    Nothing -> error "Label not defined. Should not happen after static check."

  where logSteps   = lift . mapM_ logStep
        lcheckCond = lift . checkCond
        llogInfo   = lift . logInfo

-- helper
logFromErr p from to = lift $ logError p (FromFail from to)
