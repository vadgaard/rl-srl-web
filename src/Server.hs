{-# LANGUAGE OverloadedStrings #-}

module Server (server) where

import Web.Scotty
import System.Environment
import qualified Data.Text.Lazy as TL
-- import Network.Wai.Middleware.Static

import qualified RL.Parser
import RL.Interp
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
    post "/api" $ do
      script <- TL.unpack <$> param "script" `rescue` (\_ -> return "noo script")
      interpreter <- TL.unpack <$> param "lang" `rescue` (\_ -> return "noo lang")
      case interpreter of
        "rl"  -> case RL.Parser.parseProgram script of
          Left err ->  text . TL.pack $ show err
          Right (ttab, ast) ->
            let (res, log) = RL.Interp.runProgram ast ttab 
              in case res of
                Left err -> text . TL.pack $ show err
                Right vtab -> text . TL.pack $ showTab vtab
        "srl" -> text "srl"
        other -> text . TL.pack $ other ++ " is not a valid interpreter"

    post "/translate" $ text "hello translate"
