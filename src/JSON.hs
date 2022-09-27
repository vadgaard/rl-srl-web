{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module JSON where

import Data.Aeson
import Data.Aeson.Types

import Common.Error ( Error(..) )
import Common.AST ( VarTab, showTab)
import Common.Log ( Log(..) )
import RL.AST ( RLProgram )
import SRL.AST ( SRLProgram )

badRequest :: Value
badRequest = object ["output" .= Null, "log" .= Null, "error" .= String "Bad request"]

class ToKeyValue a where
    toKeyValue :: a -> [Pair]

instance ToKeyValue Error where
    toKeyValue err = case err of
        ParseError (l,c) e ->
            ["error" .= show err, "loc_l" .= show l, "loc_c" .= show c]
        RuntimeError (l,c) e ->
            ["error" .= show err, "loc_l" .= show l, "loc_c" .= show c]
        StaticError (l,c) e ->
            ["error" .= show err, "loc_l" .= show l, "loc_c" .= show c]
        Custom e ->
            ["error" .= show err]

instance ToKeyValue RLProgram where
    toKeyValue program = ["output" .= show program]

instance ToKeyValue SRLProgram where
    toKeyValue program = ["output" .= show program]

newtype VarTabContainer = VarTabContainer VarTab
instance ToKeyValue VarTabContainer where
    toKeyValue (VarTabContainer vtab) = ["output" .= showTab vtab]

instance ToKeyValue Log where
    toKeyValue (Log vtab msgs) = ["log" .= if length msgs > 5000 then Null else (toJSON . map show) msgs]

newtype RunResult = RunResult (VarTab, Log)
instance ToKeyValue RunResult where
    toKeyValue (RunResult (vtab, log)) = toKeyValue log ++ toKeyValue (VarTabContainer vtab)

newtype ErrorResult = ErrorResult (Error, Log)
instance ToKeyValue ErrorResult where
    toKeyValue (ErrorResult (err, log)) = toKeyValue log ++ toKeyValue err

instance ToJSON RLProgram where
    toJSON program = object $ toKeyValue program ++ ["log" .= Null, "error" .= Null]

instance ToJSON SRLProgram where
    toJSON program = object $ toKeyValue program ++ ["log" .= Null, "error" .= Null]

instance ToJSON VarTabContainer where
    toJSON vtabc = object $ toKeyValue vtabc ++ ["log" .= Null,  "error" .= Null]

instance ToJSON Error where
    toJSON err = object $ ["output" .= Null, "log" .= Null] ++ toKeyValue err

instance ToJSON RunResult where
    toJSON res = object $ toKeyValue res ++ ["error" .= Null]

instance ToJSON ErrorResult where
    toJSON res = object $ toKeyValue res ++ ["output" .= Null]
