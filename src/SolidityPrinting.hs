module SolidityPrinting where
import SolidityAbstractSyntax

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

extractType :: SolidityDeclaration -> String
extractType (SolidityDeclaration SolidityString _) = "string"
extractType (SolidityDeclaration SolidityInt _) = "int"
extractType (SolidityDeclaration SolidityUInt _) = "uint"
extractType (SolidityDeclaration SolidityBool _) = "bool"
extractType (SolidityDeclaration SolidityBytes _) = "bytes"
extractType (SolidityDeclaration SolidityAddress _) = "address"
extractType _ = ""

extractName :: SolidityDeclaration -> String
extractName (SolidityDeclaration SolidityString name) = name
extractName (SolidityDeclaration SolidityInt name) = name
extractName (SolidityDeclaration SolidityUInt name) = name
extractName (SolidityDeclaration SolidityBool name) = name
extractName (SolidityDeclaration SolidityBytes name) = name
extractName (SolidityDeclaration SolidityAddress name) = name
extractName _ = ""

createDeclaration :: SolidityDeclaration -> Int -> String
createDeclaration (SolidityDeclaration SolidityString s) tabCount = (duplicate "\t" tabCount) ++ "string " ++ s ++ ";\n"  -- e.g. string value;
createDeclaration (SolidityDeclaration SolidityInt i) tabCount = (duplicate "\t" tabCount) ++ "int " ++ i ++ ";\n"  -- e.g. int value;
createDeclaration (SolidityDeclaration SolidityUInt u) tabCount = (duplicate "\t" tabCount) ++ "uint " ++ u ++ ";\n"  -- e.g. uint value;
createDeclaration (SolidityDeclaration SolidityBool b) tabCount = (duplicate "\t" tabCount) ++ "bool " ++ b ++ ";\n"  -- e.g. bool value;
createDeclaration (SolidityDeclaration SolidityBytes b) tabCount = (duplicate "\t" tabCount) ++ "bytes " ++ b ++ ";\n"  -- e.g. bool value;
createDeclaration (SolidityDeclaration SolidityAddress a) tabCount = (duplicate "\t" tabCount) ++ "address public " ++ a ++ ";\n"  -- e.g. address public minter;
createDeclaration (SolidityDeclaration (SolidityMapping t1 t2) m) tabCount = (duplicate "\t" tabCount) ++ "mapping (" ++ (extractType (SolidityDeclaration t1 "")) ++ " => " ++ (extractType (SolidityDeclaration t2 "")) ++ ") public " ++ m ++ ";\n"  -- e.g. mapping (address => uint) public balances;
createDeclaration _ _ = ""

createGetter :: SolidityDeclaration -> Int -> String
createGetter (SolidityDeclaration SolidityString s) tabCount = (duplicate "\t" tabCount) ++ "function get" ++ s ++ "() public view returns(string memory) {\n" ++ (duplicate "\t" (tabCount+1)) ++ "return " ++ s ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createGetter (SolidityDeclaration SolidityInt i) tabCount = (duplicate "\t" tabCount) ++ "function get" ++ i ++ "() public view returns(int) {\n" ++ (duplicate "\t" (tabCount+1)) ++ "return " ++ i ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createGetter (SolidityDeclaration SolidityUInt u) tabCount = (duplicate "\t" tabCount) ++ "function get" ++ u ++ "() public view returns(uint) {\n" ++ (duplicate "\t" (tabCount+1)) ++ "return " ++ u ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createGetter (SolidityDeclaration SolidityBool b) tabCount = (duplicate "\t" tabCount) ++ "function get" ++ b ++ "() public view returns(bool) {\n" ++ (duplicate "\t" (tabCount+1)) ++ "return " ++ b ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createGetter (SolidityDeclaration SolidityBytes b) tabCount = (duplicate "\t" tabCount) ++ "function get" ++ b ++ "() public view returns(bytes memory) {\n" ++ (duplicate "\t" (tabCount+1)) ++ "return " ++ b ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createGetter _ _ = ""

createSetter :: SolidityDeclaration -> Int -> String
createSetter (SolidityDeclaration SolidityString s) tabCount = (duplicate "\t" tabCount) ++ "function set" ++ s ++ "(string memory _" ++ s ++ ") public {\n" ++ (duplicate "\t" (tabCount+1)) ++ s ++ " = _" ++ s ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createSetter (SolidityDeclaration SolidityInt i) tabCount = (duplicate "\t" tabCount) ++ "function set" ++ i ++ "(int _" ++ i ++ ") public {\n" ++ (duplicate "\t" (tabCount+1)) ++ i ++ " = _" ++ i ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createSetter (SolidityDeclaration SolidityUInt u) tabCount = (duplicate "\t" tabCount) ++ "function set" ++ u ++ "(uint _" ++ u ++ ") public {\n" ++ (duplicate "\t" (tabCount+1)) ++ u ++ " = _" ++ u ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createSetter (SolidityDeclaration SolidityBool b) tabCount = (duplicate "\t" tabCount) ++ "function set" ++ b ++ "(bool _" ++ b ++ ") public {\n" ++ (duplicate "\t" (tabCount+1)) ++ b ++ " = _" ++ b ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createSetter (SolidityDeclaration SolidityBytes b) tabCount = (duplicate "\t" tabCount) ++ "function set" ++ b ++ "(bytes memory _" ++ b ++ ") public {\n" ++ (duplicate "\t" (tabCount+1)) ++ b ++ " = _" ++ b ++ ";\n" ++ (duplicate "\t" (tabCount)) ++ "}\n"
createSetter _ _ = ""

printReturn :: SolidityType -> String
printReturn Void = "public"
printReturn sv = "public view returns (" ++ (extractType (SolidityDeclaration sv "")) ++ ")"

printArgument :: SolidityDeclaration -> String
printArgument sv = (extractType sv) ++ " " ++ (extractName sv)

printAllArguments :: [SolidityDeclaration] -> String
printAllArguments [] = ""
printAllArguments [x] = (printArgument x)
printAllArguments (x:xs) = (printArgument x) ++ ", " ++ (printAllArguments xs)

printExpression :: SolidityExpression -> String
printExpression (V (SolidityVariable var)) = var
printExpression (SolidityLiteral value) = value
-- add plus, minus, mul, div...
printExpression _ = ""

printAllExpressions :: [SolidityExpression] -> String
printAllExpressions [] = ""
printAllExpressions (x:xs) = (printExpression x) ++ (printAllExpressions xs)

printStatement :: SolidityStatement -> Int -> String
printStatement (SAssign expr1 expr2) tabCount =  (duplicate "\t" tabCount) ++ (printExpression expr1) ++ " = " ++ (printExpression expr2) ++ ";\n"

printAllStatements :: [SolidityStatement] -> Int -> String
printAllStatements [] _ = ""
printAllStatements (x:xs) tabCount = (printStatement x tabCount) ++ (printAllStatements xs tabCount)

printFunc :: String -> SolidityFunction -> Int -> String
printFunc name (SolidityFunction returnType arguments statements) tabCount = (duplicate "\t" tabCount) ++ "function " ++ name ++ "(" ++ (printAllArguments arguments) ++ ") "++ (printReturn returnType) ++ " {\n" ++ (printAllStatements statements (tabCount + 1)) ++ (duplicate "\t" tabCount) ++ "}\n"

printStructVars :: [SolidityDeclaration] -> Int -> String
printStructVars [] _ = ""
printStructVars (x:xs) tabCount = (createDeclaration x tabCount) ++ (printStructVars xs tabCount)

printStruct :: String -> SolidityStruct -> Int -> String
printStruct name (SolidityStruct vars) tabCount = (duplicate "\t" tabCount) ++ "struct " ++ name ++ " {\n" ++ (printStructVars vars (tabCount + 1)) ++ (duplicate "\t" tabCount) ++ "}\n"

printDeclaration :: SolidityDeclaration -> Int -> String
printDeclaration (SolidityDeclaration SolidityString s) tabCount = createDeclaration (SolidityDeclaration SolidityString s) tabCount ++ createSetter (SolidityDeclaration SolidityString s) tabCount ++ createGetter (SolidityDeclaration SolidityString s) tabCount
printDeclaration (SolidityDeclaration SolidityInt i) tabCount = createDeclaration (SolidityDeclaration SolidityInt i) tabCount ++ createSetter (SolidityDeclaration SolidityInt i) tabCount ++ createGetter (SolidityDeclaration SolidityInt i) tabCount
printDeclaration (SolidityDeclaration SolidityUInt u) tabCount = createDeclaration (SolidityDeclaration SolidityUInt u) tabCount ++ createSetter (SolidityDeclaration SolidityUInt u) tabCount ++ createGetter (SolidityDeclaration SolidityUInt u) tabCount
printDeclaration (SolidityDeclaration SolidityBool b) tabCount = createDeclaration (SolidityDeclaration SolidityBool b) tabCount ++ createSetter (SolidityDeclaration SolidityBool b) tabCount ++ createGetter (SolidityDeclaration SolidityBool b) tabCount
printDeclaration (SolidityDeclaration SolidityBytes b) tabCount = createDeclaration (SolidityDeclaration SolidityBytes b) tabCount ++ createSetter (SolidityDeclaration SolidityBytes b) tabCount ++ createGetter (SolidityDeclaration SolidityBytes b) tabCount
printDeclaration (SolidityDeclaration SolidityAddress a) tabCount = createDeclaration (SolidityDeclaration SolidityAddress a) tabCount
printDeclaration (SolidityDeclaration (SolidityMapping t1 t2) m) tabCount = createDeclaration (SolidityDeclaration (SolidityMapping t1 t2) m) tabCount
printDeclaration (SolidityDeclaration (FunctionType f) name) tabCount = printFunc name f tabCount
printDeclaration (SolidityDeclaration (StructType s) name) tabCount = printStruct name (s) tabCount

printAllDeclarations :: [SolidityDeclaration] -> Int -> String
printAllDeclarations [] _ = ""
printAllDeclarations (x:xs) tabCount = (printDeclaration x tabCount) ++ printAllDeclarations xs tabCount

outputContract :: Contract -> String -> String -> IO ()
outputContract (Contract contractName declarations) destination version = do
  writeFile destination ""
  appendFile destination ("// SPDX-License-Identifier: GPL-3.0\n" ++ version ++ "\n\n")
  appendFile destination ("contract " ++ contractName ++ " {\n")
  appendFile destination (printAllDeclarations declarations 1)
  appendFile destination ("}\n")
