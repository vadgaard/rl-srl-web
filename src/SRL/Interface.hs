module SRL.Interface (
    runProgram,
    invertProgram,
    translateProgram
) where

import Common.Log


import qualified RL.AST as RL

import SRL.Parser
import SRL.Interp
import SRL.Inversion
import SRL.Translation

runProgram :: String -> IO (Either Error VarTab, Log)
runProgram source =
  case parseProgram source of
    Left err -> return (Left err, emptyLog)
    Right program -> return $ runAST program

invertProgram :: String -> IO (Either Error SRLProgram)
invertProgram source = return $ handleSource source invert

translateProgram :: String -> IO (Either Error RL.RLProgram)
translateProgram source = return $ handleSource source translate

handleSource :: String -> (SRLProgram -> a) -> Either Error a
handleSource source handler =
  case parseProgram source of
    Left err -> Left err
    Right program -> Right $ handler program
