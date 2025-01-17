module SolidityAbstractSyntax where

-- add type table to find out type given the var name
data Contract = Contract {
                           contractName :: String
                         , declarations :: [SolidityDeclaration]
                         }

data SolidityDeclaration = SolidityDeclaration SolidityType String -- SolidityType includes String, Uint, function types etc
data SolidityVariable = SolidityVariable String

data SolidityType = SolidityString
                  | SolidityInt
                  | SolidityUInt
                  | SolidityBool
                  | SolidityBytes
                  | SolidityAddress
                  | SolidityMapping SolidityType SolidityType
                  | Void
                  | FunctionType SolidityFunction
                  | StructType SolidityStruct

-- expressions should produce a value
data SolidityExpression = V SolidityVariable
                        | VI SolidityVariable SolidityExpression -- index into a variable
                        | SolidityLiteral String
                        | FCall String -- call to a function, how to represent function names?
                        | Plus SolidityExpression SolidityExpression
                        | Minus SolidityExpression SolidityExpression
                        | Mult SolidityExpression SolidityExpression
                        | Div SolidityExpression SolidityExpression
                        | BoolExpr BoolExpression

data BoolExpression = B SolidityExpression
                    | AND BoolExpression BoolExpression
                    | OR BoolExpression BoolExpression
                    | Equality BoolExpression BoolExpression
                    | Inequality BoolExpression BoolExpression
                    | GreaterThan BoolExpression BoolExpression
                    | LessThan BoolExpression BoolExpression
                    | GreaterThanEqualTo BoolExpression BoolExpression
                    | LessThanEqualTo BoolExpression BoolExpression

-- statements are standalone units of execution
data SolidityStatement = SAssign SolidityExpression SolidityExpression
                       | SIf IfElse
                       | SForLoop {
                                    initialization :: SolidityDeclaration
                                  , forCondition :: BoolExpression
                                  , update :: SolidityStatement
                                  , forExecutionStatements :: [SolidityStatement]
                                  }
                       | SWhileLoop
                                  {
                                    whileCondition :: BoolExpression
                                  , whileExecutionStatements :: [SolidityStatement]
                                  }
                       | SReturn

data SolidityFunction = SolidityFunction {
                                          returnType :: SolidityType
                                          , arguments :: [SolidityDeclaration]
                                          , functionalStatements :: [SolidityStatement]
                                         }

data SolidityStruct = SolidityStruct [SolidityDeclaration]

data IfElse = If BoolExpression [SolidityStatement]
            | IfElse BoolExpression  [SolidityStatement] IfElse
            | Else [SolidityStatement]
