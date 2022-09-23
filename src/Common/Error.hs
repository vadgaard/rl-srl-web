module Common.Error
( Error        (..)
, RuntimeError (..)
, StaticError  (..)
, convertParseError
, module Common.JSON
) where

import Common.AST
import Common.JSON
import qualified Text.Parsec as Parsec
import Data.List (intercalate)

data Error
  = RuntimeError Pos RuntimeError
  | ParseError   Pos String
  | StaticError  Pos StaticError
  | Custom       String

data RuntimeError
  = NonDefinedId String
  | IndexOnNonList String
  | IndexOnNonListExp
  | NonInt64Index
  | SelfAbuse Id
  | PopPushToSelf Id
  | ListInOwnIndex Id
  | DimSelfAbuse String
  | NegativeIndex
  | IndexOutOfBounds
  | PushToNonList Id
  | PopToNonEmpty Id
  | PopFromEmpty Id
  | PopFromNonList Id
  | ConflictingType Type Type
  | ConflictingTypes [Type] [Type]
  | SwapNotSameType Type Type
  | EmptyTop
  | NonListExp Type
  | NonInt64Exp Type
  | DivByZero
  | DivHasRest
  | MultByZero
  | UpdateOnNonInt64 Id Type
  | InitOnNonList String
  | InitNonEmptyList String
  | ConflictingDimensions
  | ConflictingLengths
  | NegativeDimension
  | NonInt64Dimension Type
  | FreeOnNonList String
  | FreeNonEmptyList String
  | NonReversable Id
  | StringLength String String
  | StringSuffix String String
  -- RL specific runtime errors
  | AssertionFailed Exp Bool Bool
  | FromFail String String

data StaticError
  = DuplicateLabel String
  | DuplicateVarDec String
  | DuplicateEntry
  | DuplicateExit
  | NotDefinedLabel String
  | EntryNotStart
  | ExitNotEnd
  | NoEntry
  | NoExit

instance Show Error where
  show (ParseError (l,c) e)   = e
  show (RuntimeError (l,c) e) = "A runtime error occurred at (line "++show l++", column "++show c++"):\n" ++ show e
  show (StaticError (l,c) e)  = "An error occurred at (line "++show l++", column "++show c++"): " ++ show e
  show (Custom e)             = e
instance JSON Error where
  stringify (ParseError p e)       = jsonError e p
  stringify (RuntimeError p e)     = jsonError (show $ RuntimeError p e) p
  stringify (StaticError p e)      = jsonError (show e) p
  stringify (Custom e)             = jsonError e (0,0)

instance Show RuntimeError where
  show (NonDefinedId id) = "Variable '" ++ id ++ "' is not defined."
  show (IndexOnNonList id) = "Tried indexing on non-list identifier: " ++ id
  show IndexOnNonListExp = "Tried indexing on non-list value"
  show NonInt64Index = "Tried indexing with non-integer value"
  show (SelfAbuse id) = "'" ++ show id ++ "' occurs on both sides of an update."
  show (PopPushToSelf (Id id _)) = "Variable '" ++ id ++ "' occurs in both operands in push or pop."
  show (ListInOwnIndex (Id id _)) = "List variable '" ++ id ++ "' used as index on itself in an update."
  show (DimSelfAbuse id) = "Dimension list contains variable '" ++ id ++ "' being (de)allocated."
  show NegativeIndex = "Index is negative"
  show IndexOutOfBounds = "Index out of bounds"
  show (PushToNonList idx) = "Tried pushing to non-list identifier: " ++ show idx
  show (PopToNonEmpty idx) = "Tried popping to non-clear identifier: " ++ show idx
  show (PopFromEmpty idx) = "Tried popping from empty identifier: " ++ show idx
  show (PopFromNonList idx) = "Tried popping from non-list identifier: " ++ show idx
  show (ConflictingType t1 t2) = "Expected " ++ show t1 ++ " as type, but got " ++ show t2
  show (ConflictingTypes tl1 tl2) = "Expected " ++ show tl1 ++ " as types, but got " ++ show tl2
  show (SwapNotSameType t1 t2) = "Trying to swap two variables of different types: " ++ show t1 ++ " and " ++ show t2
  show EmptyTop = "Tried reading top of empty list."
  show (NonListExp t) = "Expected list from expression, but received " ++ show t
  show (NonInt64Exp t) = "Expected " ++ show IntT ++ " from expression, but received " ++ show t
  show DivByZero = "Division by zero."
  show DivHasRest = "Division has rest."
  show MultByZero = "Multiplication update by zero."
  show (UpdateOnNonInt64 idx t) = "Tried updating non-" ++ show IntT ++ " identifier '" ++ show idx ++ "' of type " ++ show t
  show (InitOnNonList id) = "Tried initializing non-list identifier '" ++ id ++ "'"
  show (InitNonEmptyList id) = "Tried initliazing non-empty list identifier '" ++ id ++ "'"
  show ConflictingDimensions = "The number of dimensions specified does not match depth of list type."
  show ConflictingLengths = "The lengths specified do not match lengths of the of list."
  show (StringLength s t) = "Can't remove string \"" ++ t ++ "\" from \"" ++ s ++ "\" since, the first string is shorter than the second." 
  show (StringSuffix s t) = "\"" ++ s ++ "\" does not have \"" ++ t ++ "\" as suffix, thus the chosen suffix can't be removed."
  show NegativeDimension = "Encountered negative dimension"
  show (NonInt64Dimension t) = "Expected dimension size to be of type " ++ show IntT ++ ", received " ++ show t
  show (FreeOnNonList id) = "Tried freeing non-list identifier '" ++ id ++ "'."
  show (FreeNonEmptyList id) = "Tried freeing non-empty list identifier '" ++ id ++ "'."
  show (NonReversable id)    = show id ++ " is a non-reversable value."
  show (AssertionFailed exp e r) = "Assertion '" ++ show exp ++ "' expected " ++ show e ++ ", but evaluated to " ++ show r
  show (FromFail f e) = "Come-from assertion not consistent.\n Coming from " ++ f ++ ", but expected " ++ e ++ "."

instance Show StaticError where
  show (DuplicateLabel l)  = "Label '" ++ l ++ "' has been defined multiple times."
  show (DuplicateVarDec v) = "Variable '" ++ v ++ "' was declared multiple times."
  show (NotDefinedLabel l) = "Label '" ++ l ++ "' has not been defined."
  show DuplicateEntry      = "Only one entry-point is allowed."
  show DuplicateExit       = "Only one exit-point is allowed."
  show EntryNotStart       = "The entry must be at the beginning of the program."
  show ExitNotEnd          = "The exit must be at the end of the program."
  show NoEntry             = "No entry-point is specified."
  show NoExit              = "No exit-point is specified."

-- =======
-- Helpers
-- =======
convertParseError :: Parsec.ParseError -> Error
convertParseError e = ParseError ((pos . Parsec.errorPos) e) (show e)
  where pos s = (Parsec.sourceLine s, Parsec.sourceColumn s)
