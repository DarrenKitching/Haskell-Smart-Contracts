module LanguageGrammar where
import YulLanguageGrammar

data Contract = EOF
              | Pragma PragmaToken [PragmaToken] Contract -- At least one PragmaToken. Semi colon at end
              | ImportDir ImportDirective Contract
              | ContractDef ContractDefinition Contract
              | InterfaceDef InterfaceDefinition Contract
              | LibraryDef LibraryDefinition Contract
              | FunctionDef FunctionDefinition Contract
              | ConstVariableDec ConstantVariableDeclaration Contract
              | StructDef StructDefinition Contract
              | EnumDef EnumDefinition Contract

data PragmaToken = PragmaToken Char -- any char except a semicolon

data ImportDirective = ImportPath Path (Maybe Identifier) -- import path or import path as identifier
                     | ImportSymbolAlisases SymbolAliases Path -- import symbol aliases from this path
                     | ImportStar Identifier Path -- import * as identifier from path

data Path = DoubleQuotedPath StringLitDouble | SingleQuotedPath StringLitSingle -- must be non empty

data Identifier = From
                | Identifier Char [Char] -- Char can be a letter, a number, a dollar sign or an underscore

data SymbolAliases = IdentifierAlias Identifier
                   | As Identifier Identifier
                   | List [SymbolAliases]

data EnumDefinition = EnumDefinition Identifier Identifier [Identifier] -- There must be at least one identifier in the block

data Abstract

data ContractDefinition = ContractDefinition (Maybe Abstract) Identifier (Maybe [InheritanceSpecifier]) [ContractBodyElement]

data ContractBodyElement = ConstructElem ConstructorDefinition
                         | FunctionElem FunctionDefinition
                         | ModifierElem ModifierDefinition
                         | FallbackFunctionElem FallbackFunctionDefinition
                         | ReceiveFunctionElem ReceiveFunctionDefinition
                         | StructElem StructDefinition
                         | EnumElem EnumDefinition
                         | StateVariableElem StateVariableDeclaration
                         | EventElem EventDefinition
                         | UsingDirectiveElem UsingDirective

data DataLocation = Memory | Storage | CallData

data IdentifierPath = IdentifierPath Identifier [Identifier]

data Modifiers = Invocation ModifierInvocation
               | PayableModifier -- just Payable
               | Internal
               | Public

data BlockItem = EmptyBlockItem | BlockStatementItem Statement | UnCheckedBlockItem UnCheckedBlock
data Block = Block [BlockItem]
data UnCheckedBlock = UnCheckedBlock Block
data TypeName = ElementaryType ElementaryTypeName
              | FunctionType FunctionTypeName
              | Mapping MappingType
              | IdentifierPathType IdentifierPath
              | TypeNameExpression TypeName (Maybe Expression)

data Payable = Payable

data ElementaryTypeName = AddressType (Maybe Payable)
                        | Bool
                        | String
                        | Bytes
                        | SignedIntType SignedInt
                        | UnsignedIntType UnsignedInt
                        | FixedBytesType FixedBytes
                        | Fixed
                        | UFixed

data MappingType = MappingType MappingKeyType TypeName

data MappingKeyType = ElementaryMapping ElementaryTypeName
                    | IdentifierMapping IdentifierPath

data CallArgumentList = CallArgumentExpr Expression [Expression] -- one or more expressions
                      | CallArgumentExprIdent [(Identifier, Expression)] -- 0 or more Identifier Expression pairs

data ModifierInvocation = ModifierInvocation IdentifierPath (Maybe CallArgumentList)

data ParameterList = ParameterList TypeName (Maybe DataLocation) (Maybe Identifier) (Maybe ParameterList)

data ConstructorDefinition = Constructor (Maybe ParameterList) (Maybe [Modifiers]) Block -- parameter list and modifiers can be empty

data InheritanceSpecifier = InheritanceSpecifier IdentifierPath (Maybe CallArgumentList)

data InterfaceDefinition = InterfaceDefinition Identifier (Maybe [InheritanceSpecifier]) [ContractBodyElement]

data UsingDirective = UsingDirective IdentifierPath (Maybe TypeName) -- either for * or for TypeName

data Expression = Index Expression Expression
                | IndexOfIndex Expression ((Maybe Expression),(Maybe Expression)) -- double check
                | DotIdentifier Expression Identifier
                | DotAddress Expression
                | IdentifierExpression Expression [(Identifier, Expression)]
                | ExpressionArgs Expression CallArgumentList
                | PayableExpression CallArgumentList
                | Type TypeName
                | PreIncrement Expression
                | PreDecrement Expression
                | Not Expression
                | Tilda Expression -- check operator use
                | Delete Expression
                | Minus Expression
                | PostIncrement Expression
                | PostDecrement Expression
                | Pow Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Mod Expression Expression
                | Add Expression Expression
                | Sub Expression Expression
                | ArithmeticLeftShift Expression Expression
                | ArithmeticRightShift Expression Expression
                | LogicalRightShift Expression Expression
                | BitAND Expression Expression
                | Caret Expression Expression -- ^ operator -- check usage
                | BitOR Expression Expression
                | LessThan Expression Expression
                | GreaterThan Expression Expression
                | LessthanEqualTo Expression Expression
                | GreaterThanEQualTo Expression Expression
                | Equality Expression Expression
                | InEquality Expression Expression
                | AND Expression Expression -- logical and &&
                | OR Expression Expression -- logical or ||
                | InlineIf Expression Expression Expression -- question mark operator
                | Equals Expression Expression
                | OrEquals Expression Expression -- |=
                | CaretEquals Expression Expression -- ^=
                | AndEquals Expression Expression
                | LeftShiftEquals Expression Expression
                | RightShiftEquals Expression Expression
                | LogicalRightShiftEquals Expression Expression
                | PlusEquals Expression Expression
                | MinusEquals Expression Expression
                | MulEquals Expression Expression
                | DivEquals Expression Expression
                | ModEquals Expression Expression
                | New TypeName
                | TupleExpr TupleExpression
                | InlineArrayExpr InlineArrayExpression
                | IdentifierExpr Identifier
                | LiteralExpr Literal
                | ElementaryTypeNameExpr ElementaryTypeName

data Literal = StringLit StringLiteral
             | NumberLit NumberLiteral
             | BoolLit BooleanLiteral
             | HexStringLit HexStringLiteral
             | UnicodeStringLit UnicodeStringLiteral

data BooleanLiteral = True | False

data NumberLiteral = Decimal DecimalNumber (Maybe NumberUnit)
                   | HexaDecimal HexNumber (Maybe NumberUnit)

data DecimalNumber = DecimalNumber [Char] -- add in extra constraints

data HexNumber = HexNumber [Char] -- begins with 0x chars can be 0-9 A-F or underscore

data NumberUnit = Wei
                | GWei
                | Ether
                | Seconds
                | Minutes
                | Hours
                | Days
                | Weeks
                | Years

data Underscore

data HexStringLiteral = HexStringLiteral HexString [HexString] -- at least one hex-string

data HexString = DoubleQuotedHex [(Char, Char, Maybe Underscore)]
               | SingeQuoteHex [(Char, Char, Maybe Underscore)]

data TupleExpression = TupleExpression [Expression] -- list of comma seperated expressions paired together

data InlineArrayExpression = InlineArrayExpression Expression [Expression] -- at least one expression

data UnicodeStringLiteral = UnicodeStringLiteral (Maybe Char) [EscapeSequence] -- add in forwardslash unicode string lit

data FunctionTypeName = FunctionTypeName (Maybe ParameterList) ([StateMutability]) (Maybe Visibility) ([StateMutability]) (Maybe ParameterList)

data StateMutability = Pure
                     | View
                     | PayableMutability -- just payable

data Visibility = InternalVisibility
                | ExternalVisibility
                | PrivateVisibility
                | PublicVisibility

data Anonymous
data EventDefinition = EventDefinition Identifier [EventParameter] (Maybe Anonymous)
data Indexed
data EventParameter = EventParameter TypeName (Maybe Indexed) (Maybe Identifier)

data StructDefinition = StructDefinition Identifier StructMember [StructMember] -- at least one struct member
data StructMember = StructMember TypeName Identifier

data LibraryDefinition = LibraryDefinition Identifier [ContractBodyElement]

data FunctionName = IdentifierName Identifier | Fallback | Receive -- ask about this implementation
data FunctionModifiers = VisibilityModifier Visibility | StateMutabilityModifier StateMutability | ModifierInvoc ModifierInvocation | Virtual | OverrideMod OverrideSpecifier

data FunctionDefinition = FunctionaDefinition FunctionName (Maybe ParameterList) [FunctionModifiers] (Maybe ParameterList) (Maybe Block)

data OverrideSpecifier = OverrideSpecifier (Maybe [IdentifierPath])

data StateVariableModifiers = PublicState | PrivateState | InternalState | ConstantState | OverrideState OverrideSpecifier | ImmutableState

data StateVariableDeclaration = StateVariableDeclaration TypeName [StateVariableModifiers] Identifier (Maybe Expression) -- maybe equals an expression to initialse

data Statement = BlockStatement Block
               | VarDec VariableDeclarationStatement
               | ExprStatement ExpressionStatement
               | If IfStatement
               | For ForStatement
               | While WhileStatement
               | DoWhile DoWhileStatement
               | Continue ContinueStatement
               | Break BreakStatement
               | Try TryStatement
               | Return ReturnStatement
               | Emit EmitStatement
               | Assembly AssemblyStatement

data WhileStatement = WhileStatement Expression Statement
data DoWhileStatement = DoWhileStatement Statement Expression

data IfStatement = IfStatement Expression Statement (Maybe Statement) -- maybe else statement
data ForInitialiser = VarInitialse VariableDeclarationStatement
                    | ExprInitialse ExpressionStatement
                    | None
data ForStatement = ForStatement ForInitialiser (Maybe ExpressionStatement) (Maybe Expression) Statement -- Statement could be a block therefore a block of statements
data ReturnStatement = ReturnStatement (Maybe Expression)
data ExpressionStatement = ExpressionStatement Expression -- expression with semicolon after
data VariableDeclarationStatement = SingleVariableDeclartion VariableDeclaration (Maybe Expression) -- var = expression
                                  | TupleVariableDeclaration VariableDeclarationTuple Expression -- var tuple = expression

data VariableDeclaration = VariableDeclaration TypeName (Maybe DataLocation) Identifier
data Comma
data VariableDeclarationTuple = VariableDeclarationTuple [Comma] VariableDeclaration [(Maybe Comma, Maybe VariableDeclaration)]

data Evmasm
data AssemblyStatement = AssemblyStatement (Maybe Evmasm) [YulStatement]

data EmitStatement = EmitStatement Expression CallArgumentList

data TryStatement = TryStatement Expression (Maybe ParameterList) Block CatchClause [CatchClause] -- maybe parameterlist of returns. At least one catch clause
data CatchClause = CatchClause Block
                 | CatchClauseParameters (Maybe Identifier) ParameterList Block

data BreakStatement = BreakStatement -- break;
data ContinueStatement = ContinueStatement -- continue;

data ReceiveModifiers = ExternalReceive | PayableReceive | ModifierInvocReceive ModifierInvocation | VirtualReceive | OverrideReceive OverrideSpecifier
data ReceiveFunctionDefinition = ReceiveFunctionDefinition [ReceiveModifiers] (Maybe Block)
data FallBackModifier = ExternalFallBack | StateMutabilityFallBack StateMutability | ModifierInvocFallBack ModifierInvocation | VirtualFallBack | OverrideSpecFallBack OverrideSpecifier
data FallbackFunctionDefinition = FallbackFunctionDefinition ParameterList [FallBackModifier] (Maybe ParameterList) (Maybe Block)

data ModifierDefinition = ModifierDefinition Identifier (Maybe ParameterList) (Maybe OverrideSpecifier) (Maybe Block) -- if given overrideSpecifier then print virtual

data ConstantVariableDeclaration = ConstantVariableDeclaration TypeName Identifier Expression

data FixedBytes = Bytes1
                | Bytes2
                | Bytes3
                | Bytes4
                | Bytes5
                | Bytes6
                | Bytes7
                | Bytes8
                | Bytes9
                | Bytes10
                | Bytes11
                | Bytes12
                | Bytes13
                | Bytes14
                | Bytes15
                | Bytes16
                | Bytes17
                | Bytes18
                | Bytes19
                | Bytes20
                | Bytes21
                | Bytes22
                | Bytes23
                | Bytes24
                | Bytes25
                | Bytes26
                | Bytes27
                | Bytes28
                | Bytes29
                | Bytes30
                | Bytes31
                | Bytes32

data SignedInt = Int
               | Int8
               | Int16
               | Int24
               | Int32
               | Int40
               | Int48
               | Int56
               | Int64
               | Int72
               | Int80
               | Int88
               | Int96
               | Int104
               | Int112
               | Int120
               | Int128
               | Int136
               | Int144
               | Int152
               | Int160
               | Int168
               | Int176
               | Int184
               | Int192
               | Int200
               | Int208
               | Int216
               | Int224
               | Int232
               | Int240
               | Int248
               | Int256

data UnsignedInt = UInt
                 | UInt8
                 | UInt16
                 | UInt24
                 | UInt32
                 | UInt40
                 | UInt48
                 | UInt56
                 | UInt64
                 | UInt72
                 | UInt80
                 | UInt88
                 | UInt96
                 | UInt104
                 | UInt112
                 | UInt120
                 | UInt128
                 | UInt136
                 | UInt144
                 | UInt152
                 | UInt160
                 | UInt168
                 | UInt176
                 | UInt184
                 | UInt192
                 | UInt200
                 | UInt208
                 | UInt216
                 | UInt224
                 | UInt232
                 | UInt240
                 | UInt248
                 | UInt256
