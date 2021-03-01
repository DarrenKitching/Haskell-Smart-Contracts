module Abstractions where
import LanguageGrammar
import YulLanguageGrammar

-- This module contains functions for quickly generating Solidity language grammar components

createIdentifier :: String -> Identifier -- use a string to create an identifier, string must have at least one char
createIdentifier (x:xs) = Identifier x xs

createIdentifierExpression :: String -> Expression
createIdentifierExpression (xs) = IdentifierExpr $ (createIdentifier xs)

createPragma :: String -> Contract -> Contract -- use a string and other top level components to create top level pragma
createPragma (x:xs) contract = Pragma (PragmaToken x) (createPragmaTokenList xs) contract

createPragmaTokenList :: String -> [PragmaToken]
createPragmaTokenList "" = []
createPragmaTokenList (x:xs) = (PragmaToken x) : (createPragmaTokenList xs)

defineContract :: String -> [ContractBodyElement] -> Contract -> Contract
defineContract name elements contract = ContractDef (ContractDefinition (Nothing) (createIdentifier name) (Nothing) elements) contract

stateAddressDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
stateAddressDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ AddressType (Nothing)) modifiers (createIdentifier name) (Nothing)

stateBoolDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
stateBoolDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ Bool) modifiers (createIdentifier name) (Nothing)

stateStringDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
stateStringDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ String) modifiers (createIdentifier name) (Nothing)

stateBytesDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
stateBytesDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ Bytes) modifiers (createIdentifier name) (Nothing)

stateIntDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
stateIntDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ SignedIntType Int) modifiers (createIdentifier name) (Nothing)

stateUintDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
stateUintDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ UnsignedIntType UInt) modifiers (createIdentifier name) (Nothing)

stateFixedBytesDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
stateFixedBytesDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ FixedBytesType Bytes8) modifiers (createIdentifier name) (Nothing)

stateFixedDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
stateFixedDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ Fixed) modifiers (createIdentifier name) (Nothing)

stateUfixedDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
stateUfixedDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ UFixed) modifiers (createIdentifier name) (Nothing)

stateStructArrayDeclaration :: String -> [StateVariableModifiers] -> String -> StateVariableDeclaration
stateStructArrayDeclaration structName modifiers varName = StateVariableDeclaration (TypeNameExpression (IdentifierPathType (IdentifierPath (createIdentifier structName) [])) (Nothing)) modifiers (createIdentifier varName) (Nothing)

stateMappingDeclaration :: String -> MappingType -> [StateVariableModifiers] -> StateVariableDeclaration
stateMappingDeclaration name mappingTypeName modifiers  = StateVariableDeclaration (Mapping mappingTypeName) modifiers (createIdentifier name) (Nothing)

stateAddressAssignmentDeclaration :: String -> [StateVariableModifiers] -> Expression -> StateVariableDeclaration
stateAddressAssignmentDeclaration name modifiers e = StateVariableDeclaration (ElementaryType $ AddressType (Nothing)) modifiers (createIdentifier name) (Just e)

stateBoolAssignmentDeclaration :: String -> [StateVariableModifiers] -> Expression -> StateVariableDeclaration
stateBoolAssignmentDeclaration name modifiers e = StateVariableDeclaration (ElementaryType $ Bool) modifiers (createIdentifier name) (Just e)

stateStringAssignmentDeclaration :: String -> [StateVariableModifiers] -> Expression -> StateVariableDeclaration
stateStringAssignmentDeclaration name modifiers e = StateVariableDeclaration (ElementaryType $ String) modifiers (createIdentifier name) (Just e)

stateBytesAssignmentDeclaration :: String -> [StateVariableModifiers] -> Expression -> StateVariableDeclaration
stateBytesAssignmentDeclaration name modifiers e = StateVariableDeclaration (ElementaryType $ Bytes) modifiers (createIdentifier name) (Just e)

stateIntAssignmentDeclaration :: String -> [StateVariableModifiers] -> Expression -> StateVariableDeclaration
stateIntAssignmentDeclaration name modifiers e = StateVariableDeclaration (ElementaryType $ SignedIntType Int) modifiers (createIdentifier name) (Just e)

stateUintAssignmentDeclaration :: String -> [StateVariableModifiers] -> Expression -> StateVariableDeclaration
stateUintAssignmentDeclaration name modifiers e = StateVariableDeclaration (ElementaryType $ UnsignedIntType UInt) modifiers (createIdentifier name) (Just e)

stateFixedBytesAssignmentDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
stateFixedBytesAssignmentDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ FixedBytesType Bytes8) modifiers (createIdentifier name) (Nothing)

stateFixedAssignmentDeclaration :: String -> [StateVariableModifiers] -> Expression -> StateVariableDeclaration
stateFixedAssignmentDeclaration name modifiers e = StateVariableDeclaration (ElementaryType $ Fixed) modifiers (createIdentifier name) (Just e)

stateUfixedAssignmentDeclaration :: String -> [StateVariableModifiers] -> Expression -> StateVariableDeclaration
stateUfixedAssignmentDeclaration name modifiers e = StateVariableDeclaration (ElementaryType $ UFixed) modifiers (createIdentifier name) (Just e)

stateStructArrayAssignmentDeclaration :: String -> [StateVariableModifiers] -> String -> Expression -> StateVariableDeclaration
stateStructArrayAssignmentDeclaration structName modifiers varName e = StateVariableDeclaration (TypeNameExpression (IdentifierPathType (IdentifierPath (createIdentifier structName) [])) (Nothing)) modifiers (createIdentifier varName) (Just e)

stateMappingAssignmentDeclaration :: String -> MappingType -> [StateVariableModifiers] -> Expression -> StateVariableDeclaration
stateMappingAssignmentDeclaration name mappingTypeName modifiers e  = StateVariableDeclaration (Mapping mappingTypeName) modifiers (createIdentifier name) (Just e)

addressVariableDeclaration :: String -> VariableDeclarationStatement
addressVariableDeclaration name = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ AddressType (Nothing)) (Nothing) (createIdentifier name)) (Nothing)

boolVariableDeclaration :: String -> VariableDeclarationStatement
boolVariableDeclaration name = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ Bool) (Nothing) (createIdentifier name)) (Nothing)

stringVariableDeclaration :: String -> VariableDeclarationStatement
stringVariableDeclaration name = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ String) (Nothing) (createIdentifier name)) (Nothing)

bytesVariableDeclaration :: String -> VariableDeclarationStatement
bytesVariableDeclaration name = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ Bytes) (Nothing) (createIdentifier name)) (Nothing)

intVariableDeclaration :: String -> VariableDeclarationStatement
intVariableDeclaration name = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ SignedIntType Int) (Nothing) (createIdentifier name)) (Nothing)

uintVariableDeclaration :: String -> VariableDeclarationStatement
uintVariableDeclaration name = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ UnsignedIntType UInt) (Nothing) (createIdentifier name)) (Nothing)

fixedBytesVariableDeclaration :: String -> VariableDeclarationStatement
fixedBytesVariableDeclaration name = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ FixedBytesType Bytes8) (Nothing) (createIdentifier name)) (Nothing)

fixedVariableDeclaration :: String -> VariableDeclarationStatement
fixedVariableDeclaration name = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ Fixed) (Nothing) (createIdentifier name)) (Nothing)

uFixedVariableDeclaration :: String -> VariableDeclarationStatement
uFixedVariableDeclaration name = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ UFixed) (Nothing) (createIdentifier name)) (Nothing)

addressVariableAssignmentDeclaration :: String -> Expression -> VariableDeclarationStatement
addressVariableAssignmentDeclaration name e = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ AddressType (Nothing)) (Nothing) (createIdentifier name)) (Just e)

boolVariableAssignmentDeclaration :: String -> Expression -> VariableDeclarationStatement
boolVariableAssignmentDeclaration name e = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ Bool) (Nothing) (createIdentifier name)) (Just e)

stringVariableAssignmentDeclaration :: String -> Expression -> VariableDeclarationStatement
stringVariableAssignmentDeclaration name e = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ String) (Nothing) (createIdentifier name)) (Just e)

bytesVariableAssignmentDeclaration :: String -> Expression -> VariableDeclarationStatement
bytesVariableAssignmentDeclaration name e = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ Bytes) (Nothing) (createIdentifier name)) (Just e)

intVariableAssignmentDeclaration :: String -> Expression -> VariableDeclarationStatement
intVariableAssignmentDeclaration name e = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ SignedIntType Int) (Nothing) (createIdentifier name)) (Just e)

uintVariableAssignmentDeclaration :: String -> Expression -> VariableDeclarationStatement
uintVariableAssignmentDeclaration name e = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ UnsignedIntType UInt) (Nothing) (createIdentifier name)) (Just e)

fixedBytesVariableAssignmentDeclaration :: String -> Expression -> VariableDeclarationStatement
fixedBytesVariableAssignmentDeclaration name e = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ FixedBytesType Bytes8) (Nothing) (createIdentifier name)) (Just e)

fixedVariableAssignmentDeclaration :: String -> Expression -> VariableDeclarationStatement
fixedVariableAssignmentDeclaration name e = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ Fixed) (Nothing) (createIdentifier name)) (Just e)

uFixedVariableAssignmentDeclaration :: String -> Expression -> VariableDeclarationStatement
uFixedVariableAssignmentDeclaration name e = SingleVariableDeclartion (VariableDeclaration (ElementaryType $ UFixed) (Nothing) (createIdentifier name)) (Just e)

identifierPathAssignmentDeclaration :: String -> String -> Expression -> VariableDeclarationStatement
identifierPathAssignmentDeclaration identifierPath name  e = SingleVariableDeclartion (VariableDeclaration (IdentifierPathType $ IdentifierPath (createIdentifier identifierPath) []) (Just Storage) (createIdentifier name)) (Just e)

statementToBlockItem :: Statement -> BlockItem
statementToBlockItem x = BlockStatementItem x

statementsToBlockItems :: [Statement] -> [BlockItem]
statementsToBlockItems [] = []
statementsToBlockItems (x:xs) = (statementToBlockItem x) : (statementsToBlockItems xs)

createBlock :: [Statement] -> Block
createBlock statements = Block (statementsToBlockItems statements)

createParameterList :: [(ElementaryTypeName, String)] -> ParameterList
createParameterList [(elementType, "")] = ParameterList (ElementaryType $ elementType) Nothing (Nothing) (Nothing)
createParameterList ((elementType, ""):xs) = ParameterList (ElementaryType $ elementType) Nothing (Nothing) (Just $ createParameterList xs)
createParameterList [(elementType, name)] = ParameterList (ElementaryType $ elementType) Nothing (Just $ createIdentifier name) (Nothing)
createParameterList ((elementType, name):xs) = ParameterList (ElementaryType $ elementType) Nothing (Just $ createIdentifier name) (Just $ createParameterList xs)

createVoidFunction :: String -> [FunctionModifiers] -> Maybe ParameterList -> Block -> FunctionDefinition
createVoidFunction name modifiers (Just parameters) block = FunctionaDefinition (IdentifierName $ createIdentifier name) (Just parameters) modifiers (Nothing) (Just block)
createVoidFunction name modifiers (Nothing) block = FunctionaDefinition (IdentifierName $ createIdentifier name) (Nothing) modifiers (Nothing) (Just block)

createReturnFunction :: String -> [FunctionModifiers] -> Maybe ParameterList -> Block -> ParameterList -> FunctionDefinition
createReturnFunction name modifiers (Just parameters) block returnParams = FunctionaDefinition (IdentifierName $ createIdentifier name) (Just parameters) modifiers (Just returnParams) (Just block)
createReturnFunction name modifiers (Nothing) block returnParams = FunctionaDefinition (IdentifierName $ createIdentifier name) (Nothing) modifiers (Just returnParams) (Just block)

createEqualsExpression :: Identifier -> Identifier -> Expression
createEqualsExpression identifier1 identifier2 = (Equals (IdentifierExpr $ identifier1) (IdentifierExpr $ identifier2))

createEqualityExpression :: Identifier -> Identifier -> Expression
createEqualityExpression identifier1 identifier2 = (Equality (IdentifierExpr $ identifier1) (IdentifierExpr $ identifier2))

createStruct :: String -> [(ElementaryTypeName, String)] -> StructDefinition
createStruct name ((elementaryTypeName, identifier):xs) = StructDefinition (createIdentifier name) (StructMember (ElementaryType $ elementaryTypeName) (createIdentifier identifier)) (createStructMembers xs)

createStructMembers :: [(ElementaryTypeName, String)] -> [StructMember]
createStructMembers [] = []
createStructMembers ((elementaryTypeName, identifier):xs) = (StructMember (ElementaryType $ elementaryTypeName) (createIdentifier identifier)) : (createStructMembers xs)

createMappingFromType :: ElementaryTypeName -> TypeName -> MappingType
createMappingFromType elementaryTypeName typeName = MappingType (ElementaryMapping elementaryTypeName) typeName

createMappingFromIdentifier :: String -> TypeName -> MappingType
createMappingFromIdentifier name typeName = MappingType (IdentifierMapping $ IdentifierPath (createIdentifier name) []) typeName

createDecimalNumber :: [Char] -> Literal
createDecimalNumber xs = NumberLit (Decimal (DecimalNumber xs) Nothing)

createSingleQuotedStringLiteral :: [Char] -> Literal
createSingleQuotedStringLiteral xs = StringLit (StringLiteralSingle (SingleQuotedPrintable xs))

createDoubleQuotedStringLiteral :: [Char] -> Literal
createDoubleQuotedStringLiteral xs = StringLit (StringLiteralDouble (DoubleQuotePrintable xs))

createBoolLiteral :: Bool -> Literal
createBoolLiteral Prelude.True = BoolLit (LanguageGrammar.True)
createBoolLiteral Prelude.False = BoolLit (LanguageGrammar.False)

expressionToStatement :: Expression -> Statement
expressionToStatement e = ExprStatement $ ExpressionStatement e

joinConstructors :: ContractBodyElement -> ContractBodyElement -> ContractBodyElement
joinConstructors (ConstructElem (Constructor (params1) (Nothing) (block1))) (ConstructElem (Constructor (params2) (Nothing) (block2))) = ConstructElem (Constructor (joinParamLists params1 params2) (Nothing) (joinBlocks block1 block2))

joinBlocks :: Block -> Block -> Block
joinBlocks (Block xs) (Block ys) = (Block (xs ++ ys))

joinParamLists :: (Maybe ParameterList) -> (Maybe ParameterList) -> (Maybe ParameterList)
joinParamLists (Nothing) (Nothing) = Nothing
joinParamLists (Just x) (Nothing) = Just x
joinParamLists (Nothing) (Just y) = Just y
joinParamLists (Just x) (Just y) = Just (joinParams x y)

joinParams :: ParameterList -> ParameterList -> ParameterList
joinParams (ParameterList typeName (location) (identifier) (_)) (y) = ParameterList typeName (location) (identifier) (Just y)

public = VisibilityModifier PublicVisibility
view = StateMutabilityModifier View

bytes1 = FixedBytesType Bytes1
bytes2 = FixedBytesType Bytes2
bytes3 = FixedBytesType Bytes3
bytes4 = FixedBytesType Bytes4
bytes5 = FixedBytesType Bytes5
bytes6 = FixedBytesType Bytes6
bytes7 = FixedBytesType Bytes7
bytes8 = FixedBytesType Bytes8
bytes9 = FixedBytesType Bytes9
bytes10 = FixedBytesType Bytes10
bytes11 = FixedBytesType Bytes11
bytes12 = FixedBytesType Bytes12
bytes13 = FixedBytesType Bytes13
bytes14 = FixedBytesType Bytes14
bytes15 = FixedBytesType Bytes15
bytes16 = FixedBytesType Bytes16
bytes17 = FixedBytesType Bytes17
bytes18 = FixedBytesType Bytes18
bytes19 = FixedBytesType Bytes19
bytes20 = FixedBytesType Bytes20
bytes21 = FixedBytesType Bytes21
bytes22 = FixedBytesType Bytes22
bytes23 = FixedBytesType Bytes23
bytes24 = FixedBytesType Bytes24
bytes25 = FixedBytesType Bytes25
bytes26 = FixedBytesType Bytes26
bytes27 = FixedBytesType Bytes27
bytes28 = FixedBytesType Bytes28
bytes29 = FixedBytesType Bytes29
bytes30 = FixedBytesType Bytes30
bytes31 = FixedBytesType Bytes31
bytes32 = FixedBytesType Bytes32

int = SignedIntType Int
int8 = SignedIntType Int8
int16 = SignedIntType Int16
int24 = SignedIntType Int24
int32 = SignedIntType Int32
int40 = SignedIntType Int40
int48 = SignedIntType Int48
int56 = SignedIntType Int56
int64 = SignedIntType Int64
int72 = SignedIntType Int72
int80 = SignedIntType Int80
int88 = SignedIntType Int88
int96 = SignedIntType Int96
int104 = SignedIntType Int104
int112 = SignedIntType Int112
int120 = SignedIntType Int120
int128 = SignedIntType Int128
int136 = SignedIntType Int136
int144 = SignedIntType Int144
int152 = SignedIntType Int152
int160 = SignedIntType Int160
int168 = SignedIntType Int168
int176 = SignedIntType Int176
int184 = SignedIntType Int184
int192 = SignedIntType Int192
int200 = SignedIntType Int200
int208 = SignedIntType Int208
int216 = SignedIntType Int216
int224 = SignedIntType Int224
int232 = SignedIntType Int232
int240 = SignedIntType Int240
int248 = SignedIntType Int248
int256 = SignedIntType Int256

uint = UnsignedIntType UInt
uint8 = UnsignedIntType UInt8
uint16 = UnsignedIntType UInt16
uint24 = UnsignedIntType UInt24
uint32 = UnsignedIntType UInt32
uint40 = UnsignedIntType UInt40
uint48 = UnsignedIntType UInt48
uint56 = UnsignedIntType UInt56
uint64 = UnsignedIntType UInt64
uint72 = UnsignedIntType UInt72
uint80 = UnsignedIntType UInt80
uint88 = UnsignedIntType UInt88
uint96 = UnsignedIntType UInt96
uint104 = UnsignedIntType UInt104
uint112 = UnsignedIntType UInt112
uint120 = UnsignedIntType UInt120
uint128 = UnsignedIntType UInt128
uint136 = UnsignedIntType UInt136
uint144 = UnsignedIntType UInt144
uint152 = UnsignedIntType UInt152
uint160 = UnsignedIntType UInt160
uint168 = UnsignedIntType UInt168
uint176 = UnsignedIntType UInt176
uint184 = UnsignedIntType UInt184
uint192 = UnsignedIntType UInt192
uint200 = UnsignedIntType UInt200
uint208 = UnsignedIntType UInt208
uint216 = UnsignedIntType UInt216
uint224 = UnsignedIntType UInt224
uint232 = UnsignedIntType UInt232
uint240 = UnsignedIntType UInt240
uint248 = UnsignedIntType UInt248
uint256 = UnsignedIntType UInt256

address = AddressType (Nothing)
addressPayable = AddressType (Just Payable)
bool = Bool
string = String
bytes = Bytes
fixed = Fixed
ufixed = UFixed
