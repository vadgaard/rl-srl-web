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
  let (res, log) = case parseProgram source of
        Left err -> (Left err, emptyLog)
        Right program -> runAST program
    in print res >> return (res, log)

invertProgram :: String -> IO (Either Error SRLProgram)
invertProgram source =
  let res = handleSource source invert in print res >> return res

translateProgram :: String -> IO (Either Error RL.RLProgram)
translateProgram source =
  let res = handleSource source translate in print res >> return res

handleSource :: String -> (SRLProgram -> a) -> Either Error a
handleSource source handler =
  case parseProgram source of
    Left err -> Left err
    Right program -> Right $ handler program
