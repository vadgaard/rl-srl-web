module Common.JSON
( JSON
, stringify
, escStr
, esc
, jsonError
, jsonTab
, jsonCode
, getStepPos
) where

import Common.AST
import Data.List (intercalate)
import qualified Data.HashMap.Strict as M

class JSON a where
  stringify :: a -> String

escStr :: String -> String
escStr = concatMap esc

esc :: Char -> String
esc '\\' = "\\\\"
esc '\"' = "\\\""
esc '\n' = "\\n"
esc c    = [c]

-- Error
jsonError :: String -> Pos -> String
jsonError msg (l,c) =
     "{ \"type\" : \"error\", "
  ++ "\"position\" : { \"line\" : " ++ show l ++ ", \"column\" : " ++ show c ++ " }, "
  ++ "\"message\" : \"" ++ escStr msg ++ "\" }"

jsonTab :: Show a => String -> M.HashMap String a -> String
jsonTab t tab =
     "{ \"type\" : \"" ++ t ++ "\", "
  ++ "\"table\" : [" ++ (intercalate ", " . map f . sort' . M.toList) tab ++ "] }"
  where f (n,t) = "{ \"id\" : \"" ++ n ++ "\", \"value\" : \"" ++ show t ++ "\" }"

-- Code
jsonCode :: String -> String
jsonCode c =
     "{ \"type\" : \"code\", "
  ++ "\"code\" : \"" ++ escStr c ++ "\" }"
