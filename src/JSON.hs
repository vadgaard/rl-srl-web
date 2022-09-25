{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module JSON where

import Data.Aeson
    ( object, Value(Null), KeyValue((.=)), ToJSON(toJSON) )
import Data.Aeson.Types (Value( String )) 

import Common.Error ( Error(..) )
import Common.AST ( VarTab, showTab )
import RL.AST ( RLProgram )
import SRL.AST ( SRLProgram )


badRequest :: Value
badRequest = object ["error" .= String "Bad request"]

instance ToJSON RLProgram where
    toJSON program = object ["output" .= show program, "error" .= Null]

instance ToJSON SRLProgram where
    toJSON program = object ["output" .= show program, "error" .= Null]

newtype VarTabContainer = VarTabContainer VarTab
instance ToJSON VarTabContainer where
    toJSON (VarTabContainer vtab) = object ["output" .= showTab vtab, "error" .= Null]

instance ToJSON Error where
    toJSON err = case err of
        ParseError (l,c) e ->
            object ["output" .= Null, "error" .= show err, "loc_l" .= show l, "loc_c" .= show c]
        RuntimeError (l,c) e ->
            object ["output" .= Null, "error" .= show err, "loc_l" .= show l, "loc_c" .= show c]
        StaticError (l,c) e ->
            object ["output" .= Null, "error" .= show err, "loc_l" .= show l, "loc_c" .= show c]
        Custom e ->
            object ["output" .= Null, "error" .= show err]