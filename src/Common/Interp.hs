{-# LANGUAGE LambdaCase #-}

module Common.Interp (module Common.Interp, module Common.Log) where

import Common.Error
import Common.Log
import Common.AST

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
    ( ExceptT, MonadError(throwError), runExceptT )
import Control.Monad.Loops (allM, anyM)

import qualified Data.HashMap.Strict as M

-- ======================================
-- Monad transformer : The variable state
-- ======================================

type VarState = StateT VarTab (ExceptT Error (Writer [Message]))
execVarState :: VarTab -> VarState () -> (Either Error VarTab, [Message])
execVarState vtab = runWriter . runExceptT . flip execStateT vtab

rd :: Id -> Pos -> VarState Value
rd (Id name exps) p = gets (M.lookup name) >>= \case
  Just v  -> foldM getIdx v exps
  Nothing -> logError p $ NonDefinedId name

getIdx :: Value -> Exp -> VarState Value
getIdx (IntV _)      idx = logError (getExpPos idx) IndexOnNonListExp
getIdx (StringV s)   idx = eval idx >>= \case
  IntV i | i < 0                -> logError (getExpPos idx) NegativeIndex
         | n <- (fromIntegral . length) s :: Int64,
            i >= n              -> logError (getExpPos idx) IndexOutOfBounds
         | otherwise, 
            i' <- fromIntegral i -> return $ StringV $ [s !! i']
  _ -> logError (getExpPos idx) NonInt64Index
getIdx (ListV lst _) idx = eval idx >>= \case
  IntV i | i < 0     -> logError (getExpPos idx) NegativeIndex
    | otherwise -> case index lst i of
      Just v  -> return v
      Nothing -> logError (getExpPos idx) IndexOutOfBounds
  _ -> logError (getExpPos idx) NonInt64Index
  where index :: [Value] -> Int64 -> Maybe Value
        index lst' i = if fromIntegral i >= length lst' then Nothing else Just $ lst' !! fromIntegral i

logStep :: Step -> VarState ()
logStep s = do
    exec s
    msg <- gets (MsgStep s)
    tell [msg]

logInfo :: String -> VarState ()
logInfo msg = tell [MsgInfo msg]

logError :: Pos -> RuntimeError -> VarState a
logError p err' | err <- RuntimeError p err' = do
  tell [MsgError err]
  throwError err

adjust :: (Value -> Value) -> Id -> Pos -> VarState ()
adjust op (Id name []) _ = modify $ M.adjust op name
adjust op (Id name exps) p = do
  v <- rd (Id name []) p
  mn <- adjust' op exps v
  modify $ M.insert name mn

adjust' :: (Value -> Value) -> [Exp] -> Value -> VarState Value
adjust' op [] vo = return $ op vo
adjust' op (e:es) vo = do
  v <- getIdx vo e
  vi <- adjust' op es v
  case vo of
    ListV lst t -> eval e >>= \case
      IntV i -> return $ ListV (replace lst i vi) t
      _     -> logError (getExpPos e) NonInt64Index
    _ -> logError (getExpPos e) IndexOnNonListExp

  where replace :: [Value] -> Int64 -> Value -> [Value]
        replace (_:vs) 0 nv = nv:vs
        replace (v:vs) i nv = v : replace vs (i-1) nv

-- ==========
-- Statements
-- ==========
exec :: Step -> VarState ()

-- variable updates
exec (Update (Id name is) op e p) = do
  when (is `contain` name) $ logError p $ ListInOwnIndex (Id name is)
  cont <- contains2 e (Id name is) []
  when cont $ logError p $ SelfAbuse (Id name is)

  v1 <- rd (Id name is) p
  n <- case v1 of
    IntV n    -> return n
    StringV s -> return 0
    w         -> logError p $ UpdateOnNonInt64 (Id name is) (getType w)

  v2 <- eval e
  m <- case v2 of
    IntV m    -> return m
    StringV t -> return 0
    w         -> logError p $ NonInt64Exp (getType w)

  case op of
    DivEq  | m == 0       -> logError p DivByZero
           | mod n m /= 0 -> logError p DivHasRest
           | otherwise    -> return ()
    MultEq | m == 0       -> logError p MultByZero
           | otherwise    -> return ()
    _ -> return ()
  
  res <- eval $ mapUpdOp op (Lit v1 p) (Lit v2 p) p
  adjust (const res) (Id name is) p

  where
        -- =========================================================
        -- Use this instead of 'contains' for a bit more flexibility
        -- =========================================================
        contains2 :: Exp -> Id -> [Exp] -> VarState Bool
        contains2 Lit{} _ _ = return False
        contains2 (Var id2 _) (Id id1 exps1) exps2
          | id1 == id2 = if length exps1 == length exps2 then
              (==) <$> mapM eval exps1 <*> mapM eval exps2
            else return False
          | otherwise = return False
        contains2 (Binary _ e1 e2 _) name is = (||) <$> contains2 e1 name is <*> contains2 e2 name is
        contains2 (Unary Top e p) name is    = contains2 e name (Lit (IntV 0) p : is)
        contains2 (Unary Size _ _) _ _     = return False
        contains2 (Unary _ e _) name is      = contains2 e name is
        contains2 (Index l exps _) name is   =
          (||) <$> anyM (\e -> contains2 e name []) exps <*> contains2 l name (exps ++ is)
        contains2 (Parens e _) name is       = contains2 e name is

-- list modification
exec (Push id1 id2 p) = do
  when (id1 `containsId` id2) $ logError p $ PopPushToSelf id2
  when (id2 `containsId` id1) $ logError p $ PopPushToSelf id1
  when (containsItself id1)   $ logError p $ ListInOwnIndex id1
  when (containsItself id2)   $ logError p $ ListInOwnIndex id2

  v1 <- rd id1 p
  v2 <- rd id2 p

  case v2 of
    ListV ls (ListT t)
      | t == getType v1 -> do
        adjust clear id1 p
        adjust (push v1) id2 p
      | otherwise -> logError p $ ConflictingType t (getType v1)
    _ -> logError p $ PushToNonList id2

  where push v (ListV ls t) = ListV (v:ls) t
        clear (IntV _)      = IntV 0
        clear (ListV _ t)   = ListV [] t

exec (Pop id1 id2 p) = do
  when (id1 `containsId` id2) $ logError p $ PopPushToSelf id2
  when (id2 `containsId` id1) $ logError p $ PopPushToSelf id1
  when (containsItself id1)   $ logError p $ ListInOwnIndex id1
  when (containsItself id2)   $ logError p $ ListInOwnIndex id2

  v1 <- rd id1 p
  unless (isClear v1) $ logError p $ PopToNonEmpty id1
  v2 <- rd id2 p

  case v2 of
    ListV [] _ -> logError p $ PopFromEmpty id2
    ListV (v:ls) (ListT t)
      | t == getType v1 -> do
        adjust (const v) id1 p
        adjust (const $ ListV ls (ListT t)) id2 p
      | otherwise ->
        logError p $ ConflictingType t (getType v1)
    _  -> logError p $ PopFromNonList id2

-- swapping variables
exec (Swap id1 id2 p) = do
  v1 <- rd id1 p
  v2 <- rd id2 p

  let t1 = getType v1
      t2 = getType v2
  unless (t1 == t2) $ logError p $ SwapNotSameType t1 t2

  adjust (const v2) id1 p >> adjust (const v1) id2 p

-- reverse
exec (Reverse name p) = do
  rd name p >>= \case
    IntV _    -> logError p $ NonReversable name
    StringV s -> adjust (const $ StringV (reverse s)) name p
    ListV v t -> adjust (const $ ListV (reverse v) t) name p
  

-- initialising a list
exec (Init name exps p) = do
  when (exps `contain` name) $ logError p $ DimSelfAbuse name

  v <- rd (Id name []) p

  case v of
    IntV _ -> logError p $ InitOnNonList name
    ListV ls t
      | getDim t /= length exps ->
        logError p ConflictingDimensions
    _ -> return ()

  unless (isClear v) $ logError p $ InitNonEmptyList name

  nv <- foldr ((=<<) . repl) (pure $ IntV 0) exps
  adjust (const nv) (Id name []) p

  where

    repl e acc = eval e >>= \case
      IntV n
        | n >= 0 -> case acc of
          ListV _ t  -> return $ ListV (replicate (fromIntegral n) acc) (ListT t)
          IntV _     -> return $ ListV (replicate (fromIntegral n) acc) (ListT IntT)
          StringV _  -> return $ ListV (replicate (fromIntegral n) acc) (ListT StringT)
        | otherwise -> logError (getExpPos e) NegativeDimension
      StringV _ -> logError (getExpPos e) $ NonInt64Dimension StringT
      ListV _ t  -> logError (getExpPos e) $ NonInt64Dimension t

-- freeing a list
exec (Free name exps p) = do
  when (exps `contain` name) $ logError p $ DimSelfAbuse name

  v <- rd (Id name []) p

  case v of
    IntV _ -> logError p $ FreeOnNonList name
    ListV ls t
      | getDim t /= length exps ->
        logError p ConflictingDimensions
    _ -> return ()

  unless (allZero v) $ logError p $ FreeNonEmptyList name

  eql <- equalLengths v exps
  unless eql $ logError p ConflictingLengths

  adjust (const . getDefaultValue . getType $ v) (Id name []) p

  where

    equalLengths v (e:exps) = eval e >>= \case
      IntV n
        | n >= 0 -> case v of
          ListV ls t  -> (&&) (length ls == fromIntegral n) <$> allM (`equalLengths` exps) ls
          IntV _      -> logError p $ FreeOnNonList name
        | otherwise -> logError (getExpPos e) NegativeDimension
      ListV _ t -> logError (getExpPos e) $ NonInt64Dimension t
    equalLengths ListV{} [] = return False
    equalLengths IntV{}  [] = return True

-- skip
exec _ = return ()

getDim t = case t of
  ListT t -> 1 + getDim t
  IntT    -> 0


-- ===========
-- Expressions
-- ===========

eval :: Exp -> VarState Value

-- terminals
eval (Lit v _)  = return v
eval (Var name p) = rd (Id name []) p

eval (Binary op l r p)

  -- binary arithmetic and relational
  | op <= Geq  = do
    vl <- eval l
    vr <- eval r
    case (vl, vr) of
      (IntV n, IntV m) -> return $ IntV (mapBinOp op n m)
      (StringV s, StringV t) -> case op of
        Minus -> do
          unless (length t <= length s) $ logError p $ StringLength s t
          let st = reverse . take (length t) . reverse $ s
          unless (t==st) $ logError p $ StringSuffix s t
          return $ StringV $ reverse . drop (length t) . reverse $ s
        _     -> return $ StringV (mapSBinOp op s t)
      (ListV ls1 t1, ListV ls2 t2)
        | op == Equal && t1 == t2 -> return $ IntV (boolToInt $ ls1==ls2)
        | op == Neq   && t1 == t2 -> return $ IntV (boolToInt $ ls1/=ls2)
      (v,w) -> logError p $ ConflictingTypes [IntT,IntT] [getType v, getType w]

  -- binary div and mod
  | op <= Mod = do
    vl <- eval l
    vr <- eval r
    case (vl, vr) of
      (IntV n, IntV m)
        | m == 0    -> logError (getExpPos l) DivByZero
        | otherwise -> return $ IntV (mapBinOp op n m)
      (v,w)         -> logError (getExpPos l) $ ConflictingTypes [IntT,IntT] [getType v, getType w]

  -- binary logical
  | otherwise = eval l >>= \case
    IntV 0 | op==And        -> return $ IntV 0
    IntV v | v/=0 && op==Or -> return $ IntV 1
    IntV _ -> eval r >>= \case
        IntV n -> return $ IntV (if n==0 then 0 else 1)
        w      -> logError p $ NonInt64Exp (getType w)
    w -> logError p $ NonInt64Exp (getType w)

eval (Unary op exp p)

  -- unary arithmetic
  | op <= Sign = eval exp >>= \case
    IntV n -> return $ IntV (mapUnOp op n)
    w      -> logError p $ NonInt64Exp (getType w)

  -- unary logical
  | op <= Not = eval exp >>= \case
    IntV n -> return $ IntV (mapUnOp op n)
    w      -> logError p $ NonInt64Exp (getType w)

  -- unary list
  | op == Null = IntV . boolToInt . allZero <$> eval exp
  | otherwise = eval exp >>= \case
    ListV ls t -> case op of
      Top   -> case ls of
        []    -> logError p EmptyTop
        v:ls  -> return v
      Empty -> return $ IntV (boolToInt . null $ ls)
      Size  -> return $ IntV (fromIntegral . length $ ls)
    StringV s -> case op of
      Top   -> case s of
        []   -> logError p EmptyTop
        v:ls -> return $ StringV [v]
      Empty -> return $ IntV (boolToInt . null $ s)
      Size  -> return $ IntV (fromIntegral . length $ s)
    w  -> logError p $ NonListExp (getType w)

-- index
eval (Index l exps _) = do
  v <- eval l
  foldM getIdx v exps

-- parantheses
eval (Parens e _) = eval e


-- =======
-- Helpers
-- =======

-- helper for null and free
allZero :: Value -> Bool
allZero v = case v of
  ListV ls _ -> foldl (\acc e -> acc && allZero e) True ls
  IntV  n    -> n == 0

-- helper for RL and SRL interp
checkCond :: Exp -> VarState Bool
checkCond e = eval e >>= \case
  IntV q -> return $ q/=0
  w      -> logError (getExpPos e) $ ConflictingType IntT (getType w)

-- helper for some functionality
contain :: [Exp] -> String -> Bool
contain exps name = any (`contains` name) exps

contains :: Exp -> String ->  Bool
contains Lit{} _               = False
contains (Var name' _) name        = name' == name
contains (Binary _ e1 e2 _) name = e1 `contains` name || e2 `contains` name
contains (Unary _ e _) name      = e  `contains` name
contains (Index e exps _) name   = e  `contains` name || exps `contain` name
contains (Parens e _) name       = e  `contains` name

containsId :: Id -> Id -> Bool
containsId (Id name is) (Id name' _) =
  name' == name || is `contain` name'

containsItself :: Id -> Bool
containsItself (Id name is) = is `contain` name
