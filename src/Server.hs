{-# LANGUAGE OverloadedStrings #-}

module Server ( server ) where

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
      text,
      Options(..) )
import Network.Wai.Middleware.Static
    ( addBase, staticPolicy, noDots, (>->) )
import Network.Wai.Handler.Warp
    ( setPort, setTimeout, defaultSettings )
import System.Environment ( getEnv )
import System.Timeout ( timeout )
import Control.Monad.IO.Class ( liftIO )
import Control.Concurrent ( setNumCapabilities )
import Data.List ( sort )
import qualified Data.Text.Lazy as TL

import qualified RL.Interface
import qualified SRL.Interface
import JSON
    ( ErrorResult(ErrorResult),
      RunResult(RunResult),
      VarTabContainer(VarTabContainer),
      badRequest,
      requestTimeout )

timeoutTimeSec :: Int
timeoutTimeSec = 6
timeoutTimeUSec :: Int
timeoutTimeUSec = timeoutTimeSec * 1000000

server :: IO ()
server = do
  setNumCapabilities 2
  port <- read <$> getEnv "PORT"

  scotty port $ do
    -- serve frontend
    middleware $ staticPolicy (noDots >-> addBase "frontend")

    -- top domain
    get "/" $ file "./frontend/index.html"

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
            liftIO $ putStrLn "Running RL program!"
            response <- liftIO $ timeout timeoutTimeUSec $ RL.Interface.runProgram script
            case response of
              Nothing -> do
                liftIO $ putStrLn "It timed out!"
                json requestTimeout
              Just (res, trace) -> do
                liftIO $ putStrLn "Executed successfully!"
                case res of
                  Left err -> case setLog of
                    "true" -> json $ ErrorResult (err, trace)
                    _      -> json err
                  Right vtab -> case setLog of
                    "true" -> json $ RunResult (vtab, trace)
                    _      -> json $ VarTabContainer vtab
          "invert" -> do
            liftIO $ putStrLn "Inverting RL program!"
            response <- liftIO $ timeout timeoutTimeUSec $ RL.Interface.invertProgram script
            case response of
              Nothing -> do
                liftIO $ putStrLn "It timed out!"
                json requestTimeout
              Just res -> do
                liftIO $ putStrLn "Inverted successfully!"
                case res of
                  Left err -> json err
                  Right program -> json program
          "translate" -> do
            liftIO $ putStrLn "Translating RL program!"
            response <- liftIO $ timeout timeoutTimeUSec $ RL.Interface.translateProgram script
            case response of
              Nothing -> do
                liftIO $ putStrLn "It timed out!"
                json requestTimeout
              Just res -> do
                liftIO $ putStrLn "It didn't time out!"
                case res of
                  Left err -> json err
                  Right program -> json program
          _ -> json badRequest
        "srl" -> case mode of
          "run" -> do
            liftIO $ putStrLn "Running SRL program!"
            response <- liftIO $ timeout timeoutTimeUSec $ SRL.Interface.runProgram script
            case response of
              Nothing -> do
                liftIO $ putStrLn "It timed out!"
                json requestTimeout
              Just (res, trace) -> do
                liftIO $ putStrLn "Executed successfully!"
                case res of
                  Left err -> case setLog of
                    "true" -> json $ ErrorResult (err, trace)
                    _      -> json err
                  Right vtab -> case setLog of
                    "true" -> json $ RunResult (vtab, trace)
                    _      -> json $ VarTabContainer vtab
          "invert" -> do
            liftIO $ putStrLn "Inverting SRL program!"
            response <- liftIO $ timeout timeoutTimeUSec $ SRL.Interface.invertProgram script
            case response of
              Nothing -> do
                liftIO $ putStrLn "It timed out!"
                json requestTimeout
              Just res -> do
                liftIO $ putStrLn "Inverted successfully!"
                case res of
                  Left err -> json err
                  Right program -> json program
          "translate" -> do
            liftIO $ putStrLn "Translating SRL program!"
            response <- liftIO $ timeout timeoutTimeUSec $ SRL.Interface.translateProgram script
            case response of
              Nothing -> do
                liftIO $ putStrLn "It timed out!"
                json requestTimeout
              Just res -> do
                liftIO $ putStrLn "It didn't time out!"
                case res of
                  Left err -> json err
                  Right program -> json program
          _ -> json badRequest
        _ -> json badRequest

    -- nothing matches
    notFound $ text "wrong route"
