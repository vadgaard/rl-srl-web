{-# LANGUAGE OverloadedStrings #-}

module Server (server) where

import Web.Scotty
    ( file,
      get,
      json,
      middleware,
      notFound,
      param,
      post,
      rescue,
      scotty,
      text )
import Network.Wai.Middleware.Static
    ( addBase, staticPolicy, noDots, (>->))
import System.Environment ( getEnv )
import System.Directory ( listDirectory )
import System.Timeout ( timeout )
import Control.Monad.IO.Class ( liftIO )
import Data.List ( sort )
import qualified RL.Interface
import qualified SRL.Interface
import qualified Data.Text.Lazy as TL

import JSON
    ( ErrorResult(ErrorResult),
      RunResult(RunResult),
      VarTabContainer(VarTabContainer),
      badRequest,
      requestTimeout )

timeoutTime :: Int
timeoutTime = 8 * 10^6

server :: IO ()
server = do
  port <- read <$> getEnv "PORT"

  scotty port $ do
    -- serve frontend
    middleware $ staticPolicy (noDots >-> addBase "frontend")
    -- top domain
    get "/" $ file "./frontend/index.html"

    -- program list
    get "/programs" $ do
      json =<< liftIO (sort <$> listDirectory "./frontend/programs")

    -- help section
    get "/help" $ file "./frontend/help/index.html"

    -- api call
    post "/api" $ do
      lang <- TL.unpack <$> param "lang" `rescue` (\_ -> return "srl")
      script <- TL.unpack <$> param "script" `rescue` (\_ -> return "")
      mode <- TL.unpack <$> param "mode" `rescue` (\_ -> return "run")
      setLog <- TL.unpack <$> param "log" `rescue` (\_ -> return "false")
      case lang of
        "rl"  -> case mode of
          "run" -> do
            response <- liftIO $ timeout timeoutTime $ RL.Interface.runProgram script
            case response of
              Nothing -> json requestTimeout
              Just (res, trace) ->
                case res of
                  Left err -> case setLog of
                    "true" -> json $ ErrorResult (err, trace)
                    _      -> json err
                  Right vtab -> case setLog of
                    "true" -> json $ RunResult (vtab, trace)
                    _      -> json $ VarTabContainer vtab
          "invert" -> do
            response <- liftIO $ timeout timeoutTime $ RL.Interface.invertProgram script
            case response of
              Nothing -> json requestTimeout
              Just res -> case res of
                Left err -> json err
                Right program -> json program
          "translate" -> do
            response <- liftIO $ timeout timeoutTime $ RL.Interface.translateProgram script
            case response of
              Nothing -> json requestTimeout
              Just res -> case res of
                Left err -> json err
                Right program -> json program
          _ -> json badRequest
        "srl" -> case mode of
          "run" -> do
            response <- liftIO $ timeout timeoutTime $ SRL.Interface.runProgram script
            case response of
              Nothing -> json requestTimeout
              Just (res, trace) ->
                case res of
                  Left err -> case setLog of
                    "true" -> json $ ErrorResult (err, trace)
                    _      -> json err
                  Right vtab -> case setLog of
                    "true" -> json $ RunResult (vtab, trace)
                    _      -> json $ VarTabContainer vtab
          "invert" -> do
            response <- liftIO $ timeout timeoutTime $ SRL.Interface.invertProgram script
            case response of
              Nothing -> json requestTimeout
              Just res -> case res of
                Left err -> json err
                Right program -> json program
          "translate" -> do
            response <- liftIO $ timeout timeoutTime $ SRL.Interface.translateProgram script
            case response of
              Nothing -> json requestTimeout
              Just res -> case res of
                Left err -> json err
                Right program -> json program
          _ -> json badRequest
        _ -> json badRequest

    -- nothing matches
    notFound $ text "wrong route"