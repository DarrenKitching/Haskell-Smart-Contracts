module SolidityAbstractSyntax where
import Contracts

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

-- data SolidityVariable = SolidityString String | SolidityInt String | SolidityUInt String | SolidityBool String | SolidityBytes String | SolidityAddress String | SolidityMapping String String String | Void

-- expressions should produce a value
data SolidityExpression = V SolidityVariable
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
                       | SIf BoolExpression [SolidityStatement]
                       | SIfElse [BoolExpression] [[SolidityStatement]]
                       | SForLoop {
                                    initialization :: SolidityStatement
                                  , forCondition :: BoolExpression
                                  , update :: SolidityStatement
                                  , forExecutionStatements :: [SolidityStatement]
                                  }
                       | SWhileLoop
                                  {
                                    whileCondition :: BoolExpression
                                  , whileExecutionStatements :: [SolidityStatement]
                                  }
                        -- return statement

data SolidityFunction = SolidityFunction {
                                          returnType :: SolidityType
                                          , arguments :: [SolidityDeclaration]
                                          , functionalStatements :: [SolidityStatement]
                                         }

data SolidityStruct = SolidityStruct [SolidityDeclaration]
