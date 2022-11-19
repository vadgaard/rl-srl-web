module RL.Interface (
    runProgram,
    invertProgram,
    translateProgram,
    timeoutTest
) where

import Common.Log


import qualified SRL.AST as SRL

import RL.Parser
import RL.Interp
import RL.Inversion
import RL.Translation

runProgram :: String -> IO (Either Error VarTab, Log)
runProgram source =
  let res = case parseProgram source of
        Left err -> (Left err, emptyLog)
        Right program -> runAST program
    in print res >> return res

invertProgram :: String -> IO (Either Error RLProgram)
invertProgram source =
  let res = handleSource source invert in print res >> return res

translateProgram :: String -> IO (Either Error SRL.SRLProgram)
translateProgram source =
  let res = handleSource source translate in print res >> return res

handleSource :: String -> (RLProgram -> a) -> Either Error a
handleSource source handler =
  case parseProgram source of
    Left err -> Left err
    Right program -> Right $ handler program

timeoutTest :: IO (Either Error VarTab, Log)
timeoutTest = do
  print [1..]
  return $ (Right emptyVarTab, Log emptyVarTab [MsgInfo "test"])
