module LanguageGrammar where
import YulLanguageGrammar

-- This module contains a representation of the Solidity Language Grammar in Haskell

data Solidity = EOF
              | Pragma PragmaToken [PragmaToken] Solidity
              | ImportDir ImportDirective Solidity
              | ContractDef ContractDefinition Solidity
              | InterfaceDef InterfaceDefinition Solidity
              | LibraryDef LibraryDefinition Solidity
              | FunctionDef FunctionDefinition Solidity
              | ConstVariableDec ConstantVariableDeclaration Solidity
              | StructDef StructDefinition Solidity
              | EnumDef EnumDefinition Solidity

data PragmaToken = PragmaToken Char

data ImportDirective = ImportPath Path (Maybe Identifier)
                     | ImportSymbolAlisases SymbolAliases Path
                     | ImportStar Identifier Path

data Path = DoubleQuotedPath StringLitDouble | SingleQuotedPath StringLitSingle

data Identifier = From
                | Identifier Char [Char]

data SymbolAliases = IdentifierAlias Identifier
                   | As Identifier Identifier
                   | List [SymbolAliases]

data EnumDefinition = EnumDefinition Identifier Identifier [Identifier]

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
               | PayableModifier
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

data CallArgumentList = CallArgumentExpr Expression [Expression]
                      | CallArgumentExprIdent [(Identifier, Expression)]

data ModifierInvocation = ModifierInvocation IdentifierPath (Maybe CallArgumentList)

data ParameterList = ParameterList TypeName (Maybe DataLocation) (Maybe Identifier) (Maybe ParameterList)

data ConstructorDefinition = Constructor (Maybe ParameterList) (Maybe [Modifiers]) Block

data InheritanceSpecifier = InheritanceSpecifier IdentifierPath (Maybe CallArgumentList)

data InterfaceDefinition = InterfaceDefinition Identifier (Maybe [InheritanceSpecifier]) [ContractBodyElement]

data UsingDirective = UsingDirective IdentifierPath (Maybe TypeName)

data Expression = Index Expression Expression
                | IndexOfIndex Expression ((Maybe Expression),(Maybe Expression))
                | DotIdentifier Expression Identifier
                | DotAddress Expression
                | IdentifierExpression Expression [(Identifier, Expression)]
                | ExpressionArgs Expression CallArgumentList
                | PayableExpression CallArgumentList
                | Type TypeName
                | PreIncrement Expression
                | PreDecrement Expression
                | Not Expression
                | Tilda Expression
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
                | Caret Expression Expression
                | BitOR Expression Expression
                | LessThan Expression Expression
                | GreaterThan Expression Expression
                | LessthanEqualTo Expression Expression
                | GreaterThanEQualTo Expression Expression
                | Equality Expression Expression
                | InEquality Expression Expression
                | AND Expression Expression
                | OR Expression Expression
                | InlineIf Expression Expression Expression
                | Equals Expression Expression
                | OrEquals Expression Expression
                | CaretEquals Expression Expression
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

data DecimalNumber = DecimalNumber [Char]

data HexNumber = HexNumber [Char]

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

data HexStringLiteral = HexStringLiteral HexString [HexString]

data HexString = DoubleQuotedHex [(Char, Char, Maybe Underscore)]
               | SingeQuoteHex [(Char, Char, Maybe Underscore)]

data TupleExpression = TupleExpression [Expression]

data InlineArrayExpression = InlineArrayExpression Expression [Expression]

data UnicodeStringLiteral = UnicodeStringLiteral (Maybe Char) [EscapeSequence]

data FunctionTypeName = FunctionTypeName (Maybe ParameterList) ([StateMutability]) (Maybe Visibility) ([StateMutability]) (Maybe ParameterList)

data StateMutability = Pure
                     | View
                     | PayableMutability
     deriving (Eq)

data Visibility = InternalVisibility
                | ExternalVisibility
                | PrivateVisibility
                | PublicVisibility
     deriving (Eq)

data Anonymous
data EventDefinition = EventDefinition Identifier [EventParameter] (Maybe Anonymous)
data Indexed
data EventParameter = EventParameter TypeName (Maybe Indexed) (Maybe Identifier)

data StructDefinition = StructDefinition Identifier StructMember [StructMember]
data StructMember = StructMember TypeName Identifier

data LibraryDefinition = LibraryDefinition Identifier [ContractBodyElement]

data FunctionName = IdentifierName Identifier | Fallback | Receive
data FunctionModifiers = VisibilityModifier Visibility | StateMutabilityModifier StateMutability | ModifierInvoc ModifierInvocation | Virtual | OverrideMod OverrideSpecifier

data FunctionDefinition = FunctionaDefinition FunctionName (Maybe ParameterList)
  [FunctionModifiers] (Maybe ParameterList) (Maybe Block)

data OverrideSpecifier = OverrideSpecifier (Maybe [IdentifierPath])

data StateVariableModifiers = PublicState | PrivateState | InternalState | ConstantState | OverrideState OverrideSpecifier | ImmutableState

data StateVariableDeclaration = StateVariableDeclaration TypeName [StateVariableModifiers] Identifier (Maybe Expression)

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

data IfStatement = IfStatement Expression Statement (Maybe Statement)
data ForInitialiser = VarInitialse VariableDeclarationStatement
                    | ExprInitialse ExpressionStatement
                    | None
data ForStatement = ForStatement ForInitialiser (Maybe ExpressionStatement) (Maybe Expression) Statement
data ReturnStatement = ReturnStatement (Maybe Expression)
data ExpressionStatement = ExpressionStatement Expression
data VariableDeclarationStatement = SingleVariableDeclartion VariableDeclaration (Maybe Expression)
                                  | TupleVariableDeclaration VariableDeclarationTuple Expression

data VariableDeclaration = VariableDeclaration TypeName (Maybe DataLocation) Identifier
data Comma
data VariableDeclarationTuple = VariableDeclarationTuple [Comma] VariableDeclaration [(Maybe Comma, Maybe VariableDeclaration)]

data Evmasm
data AssemblyStatement = AssemblyStatement (Maybe Evmasm) [YulStatement]

data EmitStatement = EmitStatement Expression CallArgumentList

data TryStatement = TryStatement Expression (Maybe ParameterList) Block CatchClause [CatchClause]
data CatchClause = CatchClause Block
                 | CatchClauseParameters (Maybe Identifier) ParameterList Block

data BreakStatement = BreakStatement
data ContinueStatement = ContinueStatement

data ReceiveModifiers = ExternalReceive | PayableReceive | ModifierInvocReceive ModifierInvocation | VirtualReceive | OverrideReceive OverrideSpecifier
data ReceiveFunctionDefinition = ReceiveFunctionDefinition [ReceiveModifiers] (Maybe Block)
data FallBackModifier = ExternalFallBack | StateMutabilityFallBack StateMutability | ModifierInvocFallBack ModifierInvocation | VirtualFallBack | OverrideSpecFallBack OverrideSpecifier
data FallbackFunctionDefinition = FallbackFunctionDefinition ParameterList [FallBackModifier] (Maybe ParameterList) (Maybe Block)

data ModifierDefinition = ModifierDefinition Identifier (Maybe ParameterList) (Maybe OverrideSpecifier) (Maybe Block)

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
