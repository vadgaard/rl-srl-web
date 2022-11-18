
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

runProgram :: String -> (Either Error VarTab, Log)
runProgram source =
  case parseProgram source of
    Left err -> (Left err, emptyLog)
    Right program -> runAST program

invertProgram :: String -> Either Error SRLProgram
invertProgram source = handleSource source invert

translateProgram :: String -> Either Error RL.RLProgram
translateProgram source = handleSource source translate

handleSource :: String -> (SRLProgram -> a) -> Either Error a
handleSource source handler =
  case parseProgram source of
    Left err -> Left err
    Right program -> Right $ handler program
