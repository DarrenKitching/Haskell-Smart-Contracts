module Abstractions where
import LanguageGrammar
import YulLanguageGrammar

createIdentifier :: String -> Identifier -- use a string to create an identifier, string must have at least one char
createIdentifier (x:xs) = Identifier x xs

createPragma :: String -> Solidity -> Solidity -- use a string and other top level components to create top level pragma
createPragma (x:xs) solidity = Pragma (PragmaToken x) (createPragmaTokenList xs) solidity

createPragmaTokenList :: String -> [PragmaToken]
createPragmaTokenList "" = []
createPragmaTokenList (x:xs) = (PragmaToken x) : (createPragmaTokenList xs)

defineContract :: String -> [ContractBodyElement] -> Solidity -> Solidity
defineContract name elements solidity = ContractDef (ContractDefinition (Nothing) (createIdentifier name) (Nothing) elements) solidity

addressDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
addressDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ AddressType (Nothing)) modifiers (createIdentifier name) (Nothing)

boolDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
boolDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ Bool) modifiers (createIdentifier name) (Nothing)

stringDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
stringDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ String) modifiers (createIdentifier name) (Nothing)

bytesDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
bytesDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ Bytes) modifiers (createIdentifier name) (Nothing)

intDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
intDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ SignedIntType Int) modifiers (createIdentifier name) (Nothing)

uintDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
uintDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ UnsignedIntType UInt) modifiers (createIdentifier name) (Nothing)

fixedBytesDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
fixedBytesDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ FixedBytesType Bytes8) modifiers (createIdentifier name) (Nothing)

fixedDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
fixedDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ Fixed) modifiers (createIdentifier name) (Nothing)

ufixedDeclaration :: String -> [StateVariableModifiers] -> StateVariableDeclaration
ufixedDeclaration name modifiers = StateVariableDeclaration (ElementaryType $ UFixed) modifiers (createIdentifier name) (Nothing)

structArrayDeclaration :: String -> [StateVariableModifiers] -> String -> StateVariableDeclaration
structArrayDeclaration structName modifiers varName = StateVariableDeclaration (TypeNameExpression (IdentifierPathType (IdentifierPath (createIdentifier structName) [])) (Nothing)) modifiers (createIdentifier varName) (Nothing)

mappingDeclaration :: String -> MappingType -> [StateVariableModifiers] -> StateVariableDeclaration
mappingDeclaration name mappingTypeName modifiers  = StateVariableDeclaration (Mapping mappingTypeName) modifiers (createIdentifier name) (Nothing)

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

createEqualsExpression :: Identifier -> Identifier -> ExpressionStatement
createEqualsExpression identifier1 identifier2 = ExpressionStatement (Equals (IdentifierExpr $ identifier1) (IdentifierExpr $ identifier2))

createStruct :: String -> [(ElementaryTypeName, String)] -> StructDefinition
createStruct name ((elementaryTypeName, identifier):xs) = StructDefinition (createIdentifier name) (StructMember (ElementaryType $ elementaryTypeName) (createIdentifier identifier)) (createStructMembers xs)

createStructMembers :: [(ElementaryTypeName, String)] -> [StructMember]
createStructMembers [] = []
createStructMembers ((elementaryTypeName, identifier):xs) = (StructMember (ElementaryType $ elementaryTypeName) (createIdentifier identifier)) : (createStructMembers xs)

createMappingFromType :: ElementaryTypeName -> TypeName -> MappingType
createMappingFromType elementaryTypeName typeName = MappingType (ElementaryMapping elementaryTypeName) typeName

createMappingFromIdentifier :: String -> TypeName -> MappingType
createMappingFromIdentifier name typeName = MappingType (IdentifierMapping $ IdentifierPath (createIdentifier name) []) typeName

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
