module Common.Log where

import Common.AST
import Common.JSON
import Common.Error

import Data.List (intercalate)

-- ===
-- Log
-- ===

data Log = Log VarTab [Message]
instance Show Log where
  show (Log _ ms) = intercalate "\n\n" . map show $ ms

instance JSON Log where
  stringify (Log vt ms) =
    "{ \"type\" : \"log\", "
    ++ "\"state\" : " ++ jsonTab "vartab" vt ++ ", "
    ++ "\"table\" : [" ++ msgsToJSON ms ++ "]"
    ++ " }"
msgsToJSON :: [Message] -> String
msgsToJSON ms = intercalate ", " $ map jsonMsg ms
  where jsonMsg (MsgStep step vtab) =
          let (l,c) = getStepPos step in
             "{ \"type\" : \"step\", "
          ++ "\"position\" : { \"line\" : "++ show l ++ ", \"column\" : " ++ show c ++ " }, "
          ++ "\"step\" : \"" ++ (escStr . show) step ++ "\", "
          ++ "\"state\" : " ++ jsonTab "vartab" vtab ++ " }"
        jsonMsg (MsgError e) = stringify e

data Message = MsgStep   Step VarTab
             | MsgError  Error
instance Show Message where
  show (MsgStep s vtab)  =
    "line " ++ (show . fst . getStepPos) s ++ "\n> " ++
    case s of
      Skip{}  -> show s
      _       -> show s ++ "\n" ++ showTab vtab
  show (MsgError err) = "*** Error: " ++ show err
