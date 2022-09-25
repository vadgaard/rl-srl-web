# TODO
- Remove all custom JSON stuff
- Change most to show typeclass

JSON Necessary:
- RL.AST
- SRL.AST
- VarTab
- Error
- Log

For each of RL, SRL:
- Make main file exporting
    runProgram :: String -> (Either Error VarTab, Log)
    invertProgram :: String -> Either Error AST
    translateProgram :: String -> Either Error AST