{-# LANGUAGE FlexibleContexts #-}

module SRL.Translation
( translate
) where

import RL.AST as RL
import SRL.AST as SRL

import Control.Monad.Writer
import Control.Monad.State

type Labels = (RL.Label, RL.Label, RL.Label, RL.Label)
p = (0,0)

type TrlMonad = WriterT RL.AST (State Int)

genLabel :: MonadState Int m => () -> m RL.Label
genLabel () = gets ((++) "l" . show) <* modify (+1)

push :: MonadWriter RL.AST m => RL.Label -> RL.Block -> m ()
push l b = tell [(l, b)]

translate :: TypeTab -> SRL.AST -> String
translate ttab ast =
  RL.showAST ttab . evalState (execWriterT $ trlProg ast) $ 0

trlProg :: SRL.AST -> TrlMonad ()
trlProg ast = do
  l0 <- genLabel ()
  l1 <- genLabel ()
  l2 <- genLabel ()
  l3 <- genLabel ()

  let fs = Entry p
      js = Goto l1 p

      fe = From l2 p
      je = Exit p

  push l0 (fs,[],js)
  trlBlk ast (l0,l1,l2,l3)
  push l3 (fe,[],je)

trlBlk :: SRL.AST -> Labels -> TrlMonad ()
trlBlk (Step s) (l0,l1,l2,l3) = do
  let fs = From l0 p
      js = Goto l2 p

      fe = From l1 p
      je = Goto l3 p

  push l1 (fs,[s],js)
  push l2 (fe,[ ],je)

trlBlk (SRL.If t b1 b2 a) (l0,l1,l6,l7) = do
  l2 <- genLabel ()
  l3 <- genLabel ()
  l4 <- genLabel ()
  l5 <- genLabel ()

  let fs = From l0 p
      ts = RL.If t l2 l4 p

      fe = Fi a l3 l5 p
      te = Goto l7 p

  push l1 (fs,[],ts)
  trlBlk b1 (l1,l2,l3,l6)
  trlBlk b2 (l1,l4,l5,l6)
  push l6 (fe,[],te)

trlBlk (Loop a b1 b2 t) (l0,l1,l4,l7) = do
  l2 <- genLabel ()
  l3 <- genLabel ()
  l5 <- genLabel ()
  l6 <- genLabel ()

  let fs = Fi a l0 l6 p
      ts = Goto l2 p

      fe = From l3 p
      te = RL.If t l7 l5 p

  push l1 (fs,[],ts)
  trlBlk b1 (l1,l2,l3,l4)
  trlBlk b2 (l4,l5,l6,l1)
  push l4 (fe,[],te)

trlBlk (Seq b1 b2) (l0,l1,l4,l5) = do
  l2 <- genLabel ()
  l3 <- genLabel ()

  trlBlk b1 (l0,l1,l2,l3)
  trlBlk b2 (l2,l3,l4,l5)
