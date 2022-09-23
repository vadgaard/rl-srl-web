module Common.AST (module Common.AST, Int64) where

import Data.Bits (xor)
import Data.Int
import Data.Word
import Data.List (intercalate, sortBy)
import Data.Function (on)
import qualified Data.HashMap.Strict as M

-- values
data Value = IntV Int64 | StringV String | ListV [Value] Type deriving Eq
instance Show Value where
  show (IntV n)       = show n
  show (StringV s)    = show s
  show (ListV ls _)   = show ls
isClear (IntV n)      = n == 0
isClear (ListV ls _)  = null ls

-- ======
-- VarTab
-- ======

-- ids
data Id = Id String [Exp] deriving Eq
instance Show Id where
  show (Id id exps) =
    id ++ showIdx exps

showIdx exps = if null exps then "" else "[" ++ (intercalate "," . map show) exps ++ "]"

type Pos = (Int,Int)

type VarTab = M.HashMap String Value
showTab mtab =
  let tab = sort' (M.toList mtab)
      m   = if null tab then 0 else maximum . map (\(n,_) -> length n) $ tab
    in intercalate "\n" $ map (\(n,v) -> n ++ pad (m-length n+1) ++ " : " ++ show v) tab
  where pad n = replicate (n-1) ' '

sort' :: Ord a => [(a, b)] -> [(a, b)]
sort' = sortBy (compare `on` fst)

-- Statements
data Step = Update Id UpdOp Exp Pos
          | Push Id Id Pos
          | Pop  Id Id Pos
          | Skip Pos
          | Swap Id Id Pos
          | Reverse Id Pos
          | Init String [Exp] Pos
          | Free String [Exp] Pos
          deriving Eq
instance Show Step where
  show (Update id op e _) = show id ++ show op ++ show e
  show (Push id1 id2 _)   = "push " ++ show id1 ++ " " ++ show id2
  show (Pop id1 id2 _)    = "pop "  ++ show id1 ++ " " ++ show id2
  show (Init id dim _)    = "init " ++ id ++ " " ++ showIdx dim
  show (Free id dim _)    = "free " ++ id ++ " " ++ showIdx dim
  show (Swap id1 id2 _)   = "swap " ++ show id1 ++ " " ++ show id2
  show (Skip _)           = "skip"
getStepPos :: Step -> Pos
getStepPos (Update _ _ _ p) = p
getStepPos (Push _ _ p)     = p
getStepPos (Pop _ _ p)      = p
getStepPos (Swap _ _ p)     = p
getStepPos (Skip p)         = p
getStepPos (Init _ _ p)     = p
getStepPos (Free _ _ p)     = p

data UpdOp = PlusEq | MinusEq | XorEq| MultEq | DivEq deriving Eq
instance Show UpdOp where
  show PlusEq  = " += "
  show MinusEq = " -= "
  show XorEq   = " ^= "
  show MultEq  = " *= "
  show DivEq   = " /= "

data Exp
  = Lit    Value Pos
  | Var    String Pos
  | Binary BinOp Exp Exp Pos
  | Unary  UnOp  Exp Pos
  | Index  Exp [Exp] Pos
  | Parens Exp Pos
  deriving Eq
instance Show Exp where
  show (Lit v _)          = show v
  show (Var id _)         = id
  show (Binary op l r _)  = show l ++ show op ++ show r
  show (Unary  op exp _)  = show op ++ show exp
  show (Index  l exps _)  = show l ++ showIdx exps
  show (Parens exp _)     = case exp of
    Parens exp' _ -> show exp
    _             -> "("++show exp++")"
getExpPos :: Exp -> Pos
getExpPos (Lit _ p)        = p
getExpPos (Var _ p)        = p
getExpPos (Binary _ _ _ p) = p
getExpPos (Unary _ _ p)    = p
getExpPos (Index _ _ p)    = p
getExpPos (Parens _ p)     = p
showPar :: Exp -> String
showPar e = case e of
  Parens _ _ -> show e
  _          -> "(" ++ show e ++ ")"

data BinOp
  = Plus
  | Minus
  | Xor
  | Pow
  | Mult
  -- ^ Arithmetic
  -- v Relational
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  -- ^ Relational
  -- v Non-zero right arithmetic
  | Div
  | Mod
  -- ^ Arithmetic
  -- v Logical
  | Or
  | And
  deriving (Eq,Ord)
instance Show BinOp where
  show Plus    = " + "
  show Minus   = " - "
  show Xor     = " ^ "
  show Pow     = " ** "
  show Mult    = " * "
  show Div     = " / "
  show Mod     = " % "
  show Equal   = " = "
  show Neq     = " != "
  show Less    = " < "
  show Leq     = " <= "
  show Greater = " > "
  show Geq     = " >= "
  show Or      = " || "
  show And     = " && "


data UnOp
  = Neg
  | Sign
  -- ^ Arithmetic
  -- v Logical
  | Not
  -- ^ Logical
  -- v List
  | Null
  | Size
  | Empty
  | Top
  deriving (Eq,Ord)
instance Show UnOp where
  show Neg   = "neg "
  show Sign  = "sig "
  show Not   = "not "
  show Null  = "null "
  show Size  = "size "
  show Empty = "empty "
  show Top   = "top "


-- ================
-- Type declaration
-- ================

type TypeTab = [(String, Type)]
data Type = IntT | StringT | ListT Type deriving Eq
showTypeTab :: TypeTab -> String
showTypeTab = (++"\n") . concatMap (\(id,t) -> show t ++ " " ++ id ++ "\n") . sort'

hasDupDec :: TypeTab -> Maybe String
hasDupDec = hasDupDec' . map fst
  where hasDupDec' []     = Nothing
        hasDupDec' (n:ns) = if n `elem` ns then Just n else hasDupDec' ns

instance Show Type where
  show IntT       = "int"
  show StringT    = "string"
  show (ListT tp) = "list " ++ show tp
buildVTab :: TypeTab -> VarTab
buildVTab = M.map getDefaultValue . M.fromList
getType :: Value -> Type
getType (IntV _)    = IntT
getType (StringV _) = StringT
getType (ListV _ t) = t

getDefaultValue :: Type -> Value
getDefaultValue IntT    = IntV 0
getDefaultValue StringT = StringV []
getDefaultValue listt   = ListV [] listt

-- =======
-- helpers
-- =======
mapUpdOp :: UpdOp -> Exp -> Exp -> Pos -> Exp
mapUpdOp PlusEq  = Binary Plus
mapUpdOp MinusEq = Binary Minus
mapUpdOp XorEq   = Binary Xor
mapUpdOp MultEq  = Binary Mult
mapUpdOp DivEq   = Binary Div

mapBinOp Plus    = (+)
mapBinOp Minus   = (-)
mapBinOp Xor     = xor
mapBinOp Pow     = (^)
mapBinOp Mult    = (*)
mapBinOp Div     = div
mapBinOp Mod     = mod
mapBinOp Equal   = \n -> boolToInt . (n==)
mapBinOp Neq     = \n -> boolToInt . (n/=)
mapBinOp Less    = \n -> boolToInt . (n<)
mapBinOp Leq     = \n -> boolToInt . (n<=)
mapBinOp Greater = \n -> boolToInt . (n>)
mapBinOp Geq     = \n -> boolToInt . (n>=)

-- StringBinOp
mapSBinOp Plus = (++)

mapUnOp Neg  = negate
mapUnOp Sign = signum
mapUnOp Not  = boolToInt . (==0)

-- converting bool to val
boolToInt :: Bool -> Int64
boolToInt b = if b then 1 else 0
