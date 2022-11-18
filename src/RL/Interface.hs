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

runProgram :: String -> (Either Error VarTab, Log)
runProgram source =
  case parseProgram source of
    Left err -> (Left err, emptyLog)
    Right program -> runAST program

invertProgram :: String -> Either Error RLProgram
invertProgram source = handleSource source invert

translateProgram :: String -> Either Error SRL.SRLProgram
translateProgram source = handleSource source translate

handleSource :: String -> (RLProgram -> a) -> Either Error a
handleSource source handler =
  case parseProgram source of
    Left err -> Left err
    Right program -> Right $ handler program
