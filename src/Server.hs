{-# LANGUAGE OverloadedStrings #-}

module Server (server) where

import Web.Scotty
import Control.Monad (liftM)
import System.Environment
-- import Network.Wai.Middleware.Static

import qualified RL.Parser
import RL.Interp
import RL.Inversion
import RL.Translation
import qualified SRL.Parser
import SRL.Interp
import SRL.Inversion
import SRL.Translation

server :: IO ()
server = do
  port <- liftM read $ getEnv "PORT"
  scotty port $ do
    post "/run" $ do
      script <- param "script" `rescue` (\_ -> return "")
      interp <- param "lang" `rescue` (\_ -> return "")
      case interp of
        "rl"  ->
          let ast = RL.Parser.parseSrc script
          in html $ show ast
        "srl" ->
          let ast = SRL.Parser.parseSrc script
          in html $ show ast
        _ -> html "not a valid interpreter"
      
    post "/translate" $ html "hello translate"
