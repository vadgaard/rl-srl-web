{-# LANGUAGE OverloadedStrings #-}

module Server (server) where

import Web.Scotty
import System.Environment
import qualified Data.Text.Lazy as TL
import Network.Wai.Middleware.Static

import Common.AST
import qualified RL.Interface
import qualified SRL.Interface
import JSON
-- import RL.Inversion
-- import RL.Translation
-- import qualified SRL.Parser
-- import SRL.Interp
-- import SRL.Inversion
-- import SRL.Translation

server :: IO ()
server = do
  port <- read <$> getEnv "PORT"
  scotty port $ do
    -- serve frontend
    middleware $ staticPolicy (noDots >-> addBase "frontend")

    -- top domain
    get "/" $ file "./frontend/index.html"

    -- api call
    post "/api" $ do
      lang <- TL.unpack <$> param "lang" `rescue` (\_ -> return "srl")
      script <- TL.unpack <$> param "script" `rescue` (\_ -> return "")
      mode <- TL.unpack <$> param "mode" `rescue` (\_ -> return "run")
      setLog <- TL.unpack <$> param "log" `rescue` (\_ -> return "false")
      case lang of
        "rl"  -> case mode of
          "run" ->
            let (res, log) = RL.Interface.runProgram script in
            case res of
              Left err -> case setLog of
                "true" -> json $ ErrorResult (err, log) 
                _      -> json err
              Right vtab -> case setLog of
                "true" -> json $ RunResult (vtab, log) 
                _      -> json $ VarTabContainer vtab 
          "invert" ->
            case RL.Interface.invertProgram script of
              Left err -> json err
              Right program -> json program
          "translate" ->
            case RL.Interface.translateProgram script of
              Left err -> json err
              Right program -> json program
          _ -> json badRequest
        "srl" -> case mode of
          "run" ->
            let (res, log) = SRL.Interface.runProgram script in
            case res of
              Left err -> case setLog of
                "true" -> json $ ErrorResult (err, log) 
                _      -> json err
              Right vtab -> case setLog of
                "true" -> json $ RunResult (vtab, log) 
                _ -> json $ VarTabContainer vtab 
          "invert" ->
            case SRL.Interface.invertProgram script of
              Left err -> json err
              Right program -> json program
          "translate" ->
            case SRL.Interface.translateProgram script of
              Left err -> json err
              Right program -> json program
          _ -> json badRequest
        other -> json badRequest

    -- nothing matches
    notFound $ text "wrong route"
