module RL.Interface (
    runProgram,
    invertProgram,
    translateProgram
) where

import Common.Log


import qualified SRL.AST as SRL

import RL.Parser
import RL.Interp
import RL.Inversion
import RL.Translation

runProgram :: String -> IO (Either Error VarTab, Log)
runProgram source =
  case parseProgram source of
    Left err -> return (Left err, emptyLog)
    Right program -> return $ runAST program

invertProgram :: String -> IO (Either Error RLProgram)
invertProgram source = return $ handleSource source invert

translateProgram :: String -> IO (Either Error SRL.SRLProgram)
translateProgram source = return $ handleSource source translate

handleSource :: String -> (RLProgram -> a) -> Either Error a
handleSource source handler =
  case parseProgram source of
    Left err -> Left err
    Right program -> Right $ handler program
