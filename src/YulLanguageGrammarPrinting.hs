module YulLanguageGrammarPrinting where
import LanguageGrammar
import YulLanguageGrammar

-- This module allows the Haskell representation of the YUL language grammar to be converted to text for compilation

printYulStatementList :: [YulStatement] -> String
printYulStatementList [] = ""
printYulStatementList (x:xs) = (printYulStatement x) ++ (printYulStatementList xs)

printYulStatement :: YulStatement -> String
printYulStatement (YulBlockStatement yulBlock) = (printYulBlock yulBlock)
printYulStatement (YulVarDecStatement yulVariableDeclaration) = (printYulVariableDeclaration yulVariableDeclaration)
printYulStatement (YulAssignStatement yulAssignment) = (printYulAssignment yulAssignment)
printYulStatement (YulFunctionCallStatement yulFunctionalCall) = (printYulFunctionCall yulFunctionalCall)
printYulStatement (YulIf yulIfStatement) = (printYulIfStatement yulIfStatement)
printYulStatement (YulFor yulForStatement) = (printYulForStatement yulForStatement)
printYulStatement (YulSwitch yulSwitchStatement) = (printYulSwitchStatement yulSwitchStatement)
printYulStatement (YulLeaveStatement) = "leave"
printYulStatement (YulBreakStatement) = "break"
printYulStatement (YulContinueStatement) = "continue"
printYulStatement (YulFunctionDefinitionStatement yulFunctionDefinition) = (printYulFunctionDefinition yulFunctionDefinition)

printYulBlock :: YulBlock -> String
printYulBlock (YulBlock yulStatements) = "{" ++ (printYulStatementList yulStatements) ++ "}"

printYulIfStatement :: YulIfStatement -> String
printYulIfStatement (YulIfStatement yulExpression yulBlock) = "if " ++ (printYulExpression yulExpression) ++ " " ++ (printYulBlock yulBlock)

printYulForStatement :: YulForStatement -> String
printYulForStatement (YulForStatement yulBlock1 yulExpression yulBlock2 yulBlock3) = "for " ++ (printYulBlock yulBlock1) ++ (printYulExpression yulExpression) ++ (printYulBlock yulBlock2) ++ (printYulBlock yulBlock3)

printYulVariableDeclaration :: YulVariableDeclaration -> String
printYulVariableDeclaration (YulVariableDeclaration yulIdentifier (Just yulExpression)) = "let " ++ (printYulIdentifier yulIdentifier) ++ " := " ++ (printYulExpression yulExpression)
printYulVariableDeclaration (YulVariableDeclaration yulIdentifier (Nothing)) = "let " ++ (printYulIdentifier yulIdentifier)

printYulIdentifier :: YulIdentifier -> String
printYulIdentifier (YulIdentifier char chars) = [char] ++ chars

printYulExpression :: YulExpression -> String
printYulExpression (PathExpr yulPath) = (printYulPath yulPath)
printYulExpression (FuncExpr yulFunctionalCall) = (printYulFunctionCall yulFunctionalCall)
printYulExpression (LiteralYulExpr yulLiteral) = (printYulLiteral yulLiteral)

printYulPath :: YulPath -> String
printYulPath (YulPath yulIdentifier []) = (printYulIdentifier yulIdentifier)
printYulPath (YulPath yulIdentifier yulIdentifiers) = (printYulIdentifier yulIdentifier) ++ "." ++ (printYulIdentifiersList yulIdentifiers)

printYulIdentifiersList :: [YulIdentifier] -> String
printYulIdentifiersList [] = ""
printYulIdentifiersList (x:xs) = (printYulIdentifier x) ++ "." ++ (printYulIdentifiersList xs)

printYulFunctionCall :: YulFunctionalCall -> String
printYulFunctionCall (YulFunctionalCall yulIdentifier yulExpressions) = (printYulIdentifier yulIdentifier) ++ "(" ++ (printYulExpressions yulExpressions) ++ ")"
printYulFunctionCall (YulEVMCall yulEvmBuiltin yulExpressions) = (printYulEvmBuiltin yulEvmBuiltin) ++ "(" ++ (printYulExpressions yulExpressions) ++ ")"

printYulExpressions :: [YulExpression] -> String
printYulExpressions [] = ""
printYulExpressions [x] = (printYulExpression x)
printYulExpressions (x:xs) = (printYulExpression x) ++ ", " ++ (printYulExpressions xs)

printYulEvmBuiltin :: YulEvmBuiltin -> String
printYulEvmBuiltin Stop = "stop"
printYulEvmBuiltin YulAdd = "add"
printYulEvmBuiltin YulSub = "sub"
printYulEvmBuiltin YulMul = "mul"
printYulEvmBuiltin YulDiv = "div"
printYulEvmBuiltin YulSDiv = "sdiv"
printYulEvmBuiltin YulMod = "mod"
printYulEvmBuiltin YulSMod = "smod"
printYulEvmBuiltin Exp = "exp"
printYulEvmBuiltin YulNot = "not"
printYulEvmBuiltin Lt = "lt"
printYulEvmBuiltin Gt = "gt"
printYulEvmBuiltin SLt = "slt"
printYulEvmBuiltin SGt = "sgt"
printYulEvmBuiltin Eq = "eq"
printYulEvmBuiltin IsZero = "iszero"
printYulEvmBuiltin And = "and"
printYulEvmBuiltin Or = "or"
printYulEvmBuiltin Xor = "xor"
printYulEvmBuiltin Byte = "byte"
printYulEvmBuiltin Shl = "shl"
printYulEvmBuiltin Shr = "shr"
printYulEvmBuiltin Sar = "sar"
printYulEvmBuiltin AddMod = "addmod"
printYulEvmBuiltin MulMod = "mulmod"
printYulEvmBuiltin SignExtend = "signextend"
printYulEvmBuiltin Keccak256 = "keccak256"
printYulEvmBuiltin Pop = "pop"
printYulEvmBuiltin MLoad = "mload"
printYulEvmBuiltin MStore = "mstore"
printYulEvmBuiltin MStore8 = "mstore8"
printYulEvmBuiltin SLoad = "sload"
printYulEvmBuiltin SStore = "sstore"
printYulEvmBuiltin MSize = "msize"
printYulEvmBuiltin Gas = "gas"
printYulEvmBuiltin Address = "address"
printYulEvmBuiltin Balance = "balance"
printYulEvmBuiltin SelfBalance = "selfbalance"
printYulEvmBuiltin Caller = "caller"
printYulEvmBuiltin CallValue = "callvalue"
printYulEvmBuiltin CallDataLoad = "calldataload"
printYulEvmBuiltin CallDataSize = "calldatasize"
printYulEvmBuiltin CallDataCopy = "calldatacopy"
printYulEvmBuiltin ExtCodeSize = "extcodesize"
printYulEvmBuiltin ExtCodeCopy = "extcodecopy"
printYulEvmBuiltin ReturnDataSize = "returndatasize"
printYulEvmBuiltin ReturnDataCopy = "returndatacopy"
printYulEvmBuiltin ExtCodeHash = "extcodehash"
printYulEvmBuiltin Create = "create"
printYulEvmBuiltin Create2  = "create2"
printYulEvmBuiltin Call = "call"
printYulEvmBuiltin CallCode = "callcode"
printYulEvmBuiltin DelegateCall = "delegatecall"
printYulEvmBuiltin Staticcall = "staticcall"
printYulEvmBuiltin YulReturn = "return"
printYulEvmBuiltin Revert = "revert"
printYulEvmBuiltin SelfDestruct = "selfdestruct"
printYulEvmBuiltin Invalid = "invalid"
printYulEvmBuiltin Log0 = "log0"
printYulEvmBuiltin Log1 = "log1"
printYulEvmBuiltin Log2 = "log2"
printYulEvmBuiltin Log3 = "log3"
printYulEvmBuiltin Log4 = "log4"
printYulEvmBuiltin ChainID = "chainid"
printYulEvmBuiltin Origin = "origin"
printYulEvmBuiltin GasPrice = "gasprice"
printYulEvmBuiltin BlockHash = "blockhash"
printYulEvmBuiltin CoinBase = "coinbase"
printYulEvmBuiltin TimeStamp = "timestamp"
printYulEvmBuiltin Number = "number"
printYulEvmBuiltin Difficulty = "difficulty"
printYulEvmBuiltin GasLimit = "gaslimit"

printYulAssignment :: YulAssignment -> String
printYulAssignment (SingleAssign yulPath yulExpression) = (printYulPath yulPath) ++ " := " ++ (printYulExpression yulExpression)
printYulAssignment (MultiAssign yulPath [] yulFunctionalCall) = (printYulPath yulPath) ++ " := " ++ (printYulFunctionCall yulFunctionalCall)
printYulAssignment (MultiAssign yulPath yulPaths yulFunctionalCall) = (printYulPath yulPath) ++ ", " ++ (printYulPaths yulPaths) ++ " := " ++ (printYulFunctionCall yulFunctionalCall)

printYulPaths :: [YulPath] -> String
printYulPaths [] = ""
printYulPaths [x] = (printYulPath x)
printYulPaths (x:xs) = (printYulPath x) ++ ", " ++ (printYulPaths xs)

printYulSwitchStatement :: YulSwitchStatement -> String
printYulSwitchStatement (YulSwitchStatement yulExpression cases (Just yulBlock)) = "switch " ++ (printYulExpression yulExpression) ++ (printYulCases cases) ++ (printYulBlock yulBlock)
printYulSwitchStatement (YulSwitchStatement yulExpression cases (Nothing)) = "switch " ++ (printYulExpression yulExpression) ++ (printYulCases cases)
printYulSwitchStatement (YulSwitchDefaultOnly yulExpression yulBlock) = "switch " ++ (printYulExpression yulExpression) ++ " default " ++ (printYulBlock yulBlock)

printYulCase :: (YulLiteral, YulBlock) -> String
printYulCase (yulLiteral, yulBlock) = "case " ++ (printYulLiteral yulLiteral) ++ (printYulBlock yulBlock)

printYulCases :: [(YulLiteral, YulBlock)] -> String
printYulCases [] = ""
printYulCases (x:xs) = (printYulCase x) ++ (printYulCases xs)

printYulFunctionDefinition :: YulFunctionDefinition -> String
printYulFunctionDefinition (YulFunctionDefinition yulIdentifier yulIdentifiers [] yulBlock) = "function " ++ (printYulIdentifier yulIdentifier) ++ "(" ++ (printYulIdentifiersListCommas yulIdentifiers) ++ ")" ++ (printYulBlock yulBlock)
printYulFunctionDefinition (YulFunctionDefinition yulIdentifier yulIdentifiers1 yulIdentifiers2 yulBlock) = "function " ++ (printYulIdentifier yulIdentifier) ++ "(" ++ (printYulIdentifiersListCommas yulIdentifiers1) ++ ")" ++ " -> " ++ (printYulIdentifiersListCommas yulIdentifiers2) ++(printYulBlock yulBlock)

printYulIdentifiersListCommas :: [YulIdentifier] -> String
printYulIdentifiersListCommas [] = ""
printYulIdentifiersListCommas [x] = (printYulIdentifier x)
printYulIdentifiersListCommas (x:xs) = (printYulIdentifier x) ++ ", " ++ (printYulIdentifiersListCommas xs)

printYulLiteral :: YulLiteral -> String
printYulLiteral (DecNum yulDecimalNumber) = (printYulDecimalNumber yulDecimalNumber)
printYulLiteral (YulString yulStringLiteral) = (printYulStringLiteral yulStringLiteral)
printYulLiteral (HexNum yulHexNumber) = (printYulHexNumber yulHexNumber)
printYulLiteral (YulBool yulBoolean) = (printYulBoolean yulBoolean)

printYulBoolean :: YulBoolean -> String
printYulBoolean (YulTrue) = "true"
printYulBoolean (YulFalse) = "false"

printYulDecimalNumber :: YulDecimalNumber -> String
printYulDecimalNumber (YulDecimalNumber char chars) = [char] ++ chars

printYulHexNumber :: YulHexNumber -> String
printYulHexNumber (YulHexNumber char chars) = "0x" ++ [char] ++ chars

printYulStringLiteral :: YulStringLiteral -> String
printYulStringLiteral (YulStringLiteralDouble stringLitDouble) = (printStringLitDouble stringLitDouble)
printYulStringLiteral (YulStringLiteralSingle stringLitSingle) = (printStringLitSingle stringLitSingle)

printStringLitDouble :: StringLitDouble -> String
printStringLitDouble (DoubleQuotePrintable doubleQuotePrintable) = doubleQuotePrintable
printStringLitDouble (Double escapeSequence) = printEscapeSequence escapeSequence

printStringLitSingle :: StringLitSingle -> String
printStringLitSingle (SingleQuotedPrintable singleQuotedPrintable) = singleQuotedPrintable
printStringLitSingle (Single escapeSequence) = printEscapeSequence escapeSequence

printEscapeSequence :: EscapeSequence -> String
printEscapeSequence (U c1 c2 c3 c4) = "\\u" ++ [c1] ++ [c2] ++ [c3] ++ [c4]
printEscapeSequence (X c1 c2) = "\\x" ++ [c1] ++ [c2]
printEscapeSequence (Simple char) = "\\" ++ [char]
