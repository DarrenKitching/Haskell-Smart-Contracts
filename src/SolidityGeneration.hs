module SolidityGeneration where
import Contracts


-- add type table to find out type given the var name
data Contract = Contract {
                           contractName :: String
                         , declarations :: [SolidityDeclaration]
                         }

data SolidityDeclaration = SolidityDeclaration String  -- SolidityType includes String, Uint, function types etc
data SolidityVariable = SolidityString String | SolidityInt String | SolidityUInt String | SolidityBool String | SolidityBytes String | SolidityAddress String | SolidityMapping String String String | Void

-- expressions should produce a value
data SolidityExpression = V SolidityVariable
                        | SolidityLiteral String
                        | FCall SolidityFunction -- call to a function
                        | Plus SolidityExpression SolidityExpression
                        | Minus SolidityExpression SolidityExpression
                        | Mult SolidityExpression SolidityExpression
                        | Div SolidityExpression SolidityExpression
                        | BoolExpr BoolExpression

-- statements are standalone units of execution
data SolidityStatement = SolidityExpression -- contains if and if-else...
                       | SAssign SolidityExpression SolidityExpression
                       | SIf BoolExpression [SolidityStatement]
                       | SIfElse [BoolExpression] [[SolidityStatement]]

data SolidityFunction = SolidityFunction { functionName :: String
                                         , arguments :: [SolidityVariable]
                                         , returnType :: SolidityVariable
                                         , statements :: [SolidityStatement]
                                         }

data SolidityStruct = SolidityStruct String [SolidityVariable] -- Struct name and list of variables in the Struct

data BoolExpression = B SolidityExpression
                    | AND BoolExpression BoolExpression
                    | OR BoolExpression BoolExpression
                    | Equality BoolExpression BoolExpression
                    | Inequality BoolExpression BoolExpression
                    | GreaterThan BoolExpression BoolExpression
                    | LessThan BoolExpression BoolExpression
                    | GreaterThanEqualTo BoolExpression BoolExpression
                    | LessThanEqualTo BoolExpression BoolExpression

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

extractType :: SolidityVariable -> String
extractType (SolidityString _) = "string"
extractType (SolidityInt _) = "int"
extractType (SolidityUInt _) = "uint"
extractType (SolidityBool _) = "bool"
extractType (SolidityBytes _) = "bytes"
extractType (SolidityAddress _) = "address"
extractType _ = ""

extractName :: SolidityVariable -> String
extractName (SolidityString name) = name
extractName (SolidityInt name) = name
extractName (SolidityUInt name) = name
extractName (SolidityBool name) = name
extractName (SolidityBytes name) = name
extractName (SolidityAddress name) = name
extractName _ = ""

createVariable :: SolidityVariable -> Int -> String
createVariable (SolidityString s) tabCount = (duplicate "\t" tabCount) ++ "string " ++ s ++ ";\n"  -- e.g. string value;
createVariable (SolidityInt i) tabCount = (duplicate "\t" tabCount) ++ "int " ++ i ++ ";\n"  -- e.g. int value;
createVariable (SolidityUInt u) tabCount = (duplicate "\t" tabCount) ++ "uint " ++ u ++ ";\n"  -- e.g. uint value;
createVariable (SolidityBool b) tabCount = (duplicate "\t" tabCount) ++ "bool " ++ b ++ ";\n"  -- e.g. bool value;
createVariable (SolidityBytes b) tabCount = (duplicate "\t" tabCount) ++ "bytes " ++ b ++ ";\n"  -- e.g. bool value;
createVariable (SolidityAddress a) tabCount = (duplicate "\t" tabCount) ++ "address public " ++ a ++ ";\n"  -- e.g. address public minter;
createVariable (SolidityMapping t1 t2 m) tabCount = (duplicate "\t" tabCount) ++ "mapping (" ++ t1 ++ " => " ++ t2 ++ ") public " ++ m ++ ";\n"  -- e.g. mapping (address => uint) public balances;
createVariable _ _ = ""

createGetter :: SolidityVariable -> Int -> String
createGetter (SolidityString s) tabCount = (duplicate "\t" tabCount) ++ "function get" ++ s ++ "() public view returns(string memory) {\n" ++ (duplicate "\t" (tabCount+1)) ++ "return " ++ s ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createGetter (SolidityInt i) tabCount = (duplicate "\t" tabCount) ++ "function get" ++ i ++ "() public view returns(int) {\n" ++ (duplicate "\t" (tabCount+1)) ++ "return " ++ i ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createGetter (SolidityUInt u) tabCount = (duplicate "\t" tabCount) ++ "function get" ++ u ++ "() public view returns(uint) {\n" ++ (duplicate "\t" (tabCount+1)) ++ "return " ++ u ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createGetter (SolidityBool b) tabCount = (duplicate "\t" tabCount) ++ "function get" ++ b ++ "() public view returns(bool) {\n" ++ (duplicate "\t" (tabCount+1)) ++ "return " ++ b ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createGetter (SolidityBytes b) tabCount = (duplicate "\t" tabCount) ++ "function get" ++ b ++ "() public view returns(bytes memory) {\n" ++ (duplicate "\t" (tabCount+1)) ++ "return " ++ b ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createGetter _ _ = ""

createSetter :: SolidityVariable -> Int -> String
createSetter (SolidityString s) tabCount = (duplicate "\t" tabCount) ++ "function set" ++ s ++ "(string memory _" ++ s ++ ") public {\n" ++ (duplicate "\t" (tabCount+1)) ++ s ++ " = _" ++ s ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createSetter (SolidityInt i) tabCount = (duplicate "\t" tabCount) ++ "function set" ++ i ++ "(int _" ++ i ++ ") public {\n" ++ (duplicate "\t" (tabCount+1)) ++ i ++ " = _" ++ i ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createSetter (SolidityUInt u) tabCount = (duplicate "\t" tabCount) ++ "function set" ++ u ++ "(uint _" ++ u ++ ") public {\n" ++ (duplicate "\t" (tabCount+1)) ++ u ++ " = _" ++ u ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createSetter (SolidityBool b) tabCount = (duplicate "\t" tabCount) ++ "function set" ++ b ++ "(bool _" ++ b ++ ") public {\n" ++ (duplicate "\t" (tabCount+1)) ++ b ++ " = _" ++ b ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createSetter (SolidityBytes b) tabCount = (duplicate "\t" tabCount) ++ "function set" ++ b ++ "(bytes memory _" ++ b ++ ") public {\n" ++ (duplicate "\t" (tabCount+1)) ++ b ++ " = _" ++ b ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createSetter _ _ = ""

printGlobalVariables :: [SolidityVariable] -> Int -> String
printGlobalVariables [] _ = ""
printGlobalVariables (x:xs) tabCount = (createVariable x tabCount) ++ (createGetter x tabCount) ++ (createSetter x tabCount) ++ (printGlobalVariables xs tabCount)

printStructVars :: [SolidityVariable] -> Int -> String
printStructVars [] _ = ""
printStructVars (x:xs) tabCount = (createVariable x tabCount) ++ (printStructVars xs tabCount)

printStruct :: SolidityStruct -> Int -> String
printStruct (SolidityStruct name vars) tabCount = (duplicate "\t" tabCount) ++ "struct " ++ name ++ " {\n" ++ (printStructVars vars (tabCount + 1)) ++ (duplicate "\t" tabCount) ++ "}\n"

printAllStructs :: [SolidityStruct] -> Int -> String
printAllStructs [] _ = ""
printAllStructs (x:xs) tabCount = (printStruct x tabCount) ++ (printAllStructs xs tabCount)

printReturn :: SolidityVariable -> String
printReturn Void = "public"
printReturn sv = "public view returns (" ++ (extractType sv) ++ ")"

printArgument :: SolidityVariable -> String
printArgument sv = (extractType sv) ++ " " ++ (extractName sv)

printAllArguments :: [SolidityVariable] -> String
printAllArguments [] = ""
printAllArguments [x] = (printArgument x)
printAllArguments (x:xs) = (printArgument x) ++ ", " ++ (printAllArguments xs)

printExpression :: SolidityExpression -> String
printExpression (V var) = extractName var
printExpression (SolidityLiteral value) = value
-- add plus, minus, mul, div...
printExpression _ = ""

printAllExpressions :: [SolidityExpression] -> String
printAllExpressions [] = ""
printAllExpressions (x:xs) = (printExpression x) ++ (printAllExpressions xs)

printStatement :: SolidityStatement -> Int -> String
printStatement (SAssign expr1 expr2) tabCount =  (duplicate "\t" tabCount) ++ (printExpression expr1) ++ " " ++ (printExpression expr2) ++ ";"

printAllStatements :: [SolidityStatement] -> Int -> String
printAllStatements [] _ = ""
printAllStatements (x:xs) tabCount = (printStatement x tabCount) ++ (printAllStatements xs tabCount)

printFunc :: SolidityFunction -> Int -> String
printFunc (SolidityFunction name arguments returnType statements) tabCount = (duplicate "\t" tabCount) ++ "function " ++ name ++ "(" ++ (printAllArguments arguments) ++ ") "++ (printReturn returnType) ++ " {\n" ++ (printAllStatements statements tabCount) ++ (duplicate "\t" tabCount) ++ "}\n"

printAllFuncs :: [SolidityFunction] -> Int -> String
printAllFuncs [] _ = ""
printAllFuncs (x:xs) tabCount = (printFunc x tabCount) ++ (printAllFuncs xs tabCount)

outputContract :: [SolidityVariable] -> [SolidityStruct] -> [SolidityFunction] -> String -> String -> String -> IO ()
outputContract vars structs funcs name destination version = do
  writeFile destination ""
  appendFile destination ("// SPDX-License-Identifier: GPL-3.0\n" ++ version ++ "\n\n")
  appendFile destination ("contract " ++ name ++ " {\n")
  appendFile destination (printAllStructs structs 1)
  appendFile destination (printGlobalVariables vars 1)
  appendFile destination (printAllFuncs funcs 1)
  appendFile destination ("}\n")
