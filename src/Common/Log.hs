module Common.Log where

import Common.AST
import Common.Error

import Data.List (intercalate)

-- ===
-- Log
-- ===

data Log = Log VarTab [Message]
instance Show Log where
  show (Log _ ms) = intercalate "\n\n" . map show $ ms

emptyLog :: Log
emptyLog = Log emptyVarTab []

data Message = MsgStep   Step VarTab
             | MsgInfo   String
             | MsgError  Error
instance Show Message where
  show (MsgStep s vtab)  =
    "line " ++ (show . fst . getStepPos) s ++ "\n> " ++
    case s of
      Skip{}  -> show s
      _       -> show s ++ "\n" ++ showTab vtab
  show (MsgInfo msg) = msg
  show (MsgError err) = "*** Error: " ++ show err
