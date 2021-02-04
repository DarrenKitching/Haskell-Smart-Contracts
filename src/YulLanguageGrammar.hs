module YulLanguageGrammar where

data StringLitDouble = DoubleQuotePrintable DoubleQuotePrintable | Double EscapeSequence
data StringLitSingle = SingleQuotedPrintable SingleQuotedPrintable | Single EscapeSequence
data StringLiteral = StringLiteralDouble StringLitDouble
                   | StringLiteralSingle StringLitSingle
type DoubleQuotePrintable = Char -- need rule that this char can't be a backslash or double quotes backslash
type SingleQuotedPrintable = Char -- need ruel that this char can't be a single quote or backslash
data EscapeSequence = U Char Char Char Char -- additional rules needed
                   | X Char Char
                   | Simple Char

data YulStatement = YulBlockStatement YulBlock
                  | YulVarDecStatement YulVariableDeclaration
                  | YulAssignStatement YulAssignment
                  | YulFunctionCallStatement YulFunctionalCall
                  | YulIf YulIfStatement
                  | YulFor YulForStatement
                  | YulSwitch YulSwitchStatement
                  | YulLeaveStatement
                  | YulBreakStatement
                  | YulContinueStatement
                  | YulFunctionDefinitionStatement YulFunctionDefinition

data YulBlock = YulBlock [YulStatement]
data YulVariableDeclaration = YulVariableDeclaration YulIdentifier (Maybe YulExpression)
data YulVariableDeclarationTuple = YulVariableDeclarationTuple YulIdentifier [YulIdentifier] YulFunctionalCall -- at least one YulIdentifier

data YulAssignment = SingleAssign YulPath YulExpression
                   | MultiAssign YulPath [YulPath] YulFunctionalCall

data YulFunctionalCall = YulFunctionalCall YulIdentifier [YulExpression]
                       | YulEVMCall YulEvmBuiltin [YulExpression]

data YulIfStatement = YulIfStatement YulExpression YulBlock

data YulForStatement = YulForStatement YulBlock YulExpression YulBlock YulBlock

data YulSwitchStatement = YulSwitchStatement YulExpression [(YulLiteral, YulBlock)] (Maybe YulBlock)
                        | YulSwitchDefaultOnly YulExpression YulBlock

data YulFunctionDefinition = YulFunctionDefinition YulIdentifier [YulIdentifier] [YulIdentifier] YulBlock

data YulIdentifier = YulIdentifier Char [Char] -- at least one char

data YulExpression = PathExpr YulPath | FuncExpr YulFunctionalCall | LiteralYulExpr YulLiteral

data YulPath = YulPath YulIdentifier [YulIdentifier] -- at least one identifier

data YulEvmBuiltin = Stop
                   | YulAdd
                   | YulSub
                   | YulMul
                   | YulDiv
                   | YulSDiv
                   | YulMod
                   | YulSMod
                   | Exp
                   | YulNot
                   | Lt
                   | Gt
                   | SLt
                   | SGt
                   | Eq
                   | IsZero
                   | And
                   | Or
                   | Xor
                   | Byte
                   | Shl
                   | Shr
                   | Sar
                   | AddMod
                   | MulMod
                   | SignExtend
                   | Keccak256
                   | Pop
                   | MLoad
                   | MStore
                   | MStore8
                   | SLoad
                   | SStore
                   | MSize
                   | Gas
                   | Address
                   | Balance
                   | SelfBalance
                   | Caller
                   | CallValue
                   | CallDataLoad
                   | CallDataSize
                   | CallDataCopy
                   | ExtCodeSize
                   | ExtCodeCopy
                   | ReturnDataSize
                   | ReturnDataCopy
                   | ExtCodeHash
                   | Create
                   | Create2
                   | Call
                   | CallCode
                   | DelegateCall
                   | Staticcall
                   | YulReturn
                   | Revert
                   | SelfDestruct
                   | Invalid
                   | Log0
                   | Log1
                   | Log2
                   | Log3
                   | Log4
                   | ChainID
                   | Origin
                   | GasPrice
                   | BlockHash
                   | CoinBase
                   | TimeStamp
                   | Number
                   | Difficulty
                   | GasLimit

data YulLiteral = DecNum YulDecimalNumber | YulString YulStringLiteral | HexNum YulHexNumber | YulBool YulBoolean

data YulBoolean = YulTrue | YulFalse

data YulDecimalNumber = YulDecimalNumber Char [Char] -- zero or a non 0 digit followed by any size list of digits

data YulHexNumber = YulHexNumber Char [Char] -- 0x then list of 0-9 or A-F chars

data YulStringLiteral = YulStringLiteralDouble StringLitDouble
                      | YulStringLiteralSingle StringLitSingle
