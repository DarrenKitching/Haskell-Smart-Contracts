module LanguageGrammarPrinting where
import LanguageGrammar
import YulLanguageGrammar

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

printSolidity :: Solidity -> String
printSolidity (EOF) = ""
printSolidity (Pragma token tokenList nextHighLevel) = (printPragmaToken token) ++ (printPragmaTokenList tokenList) ++ ";\n" ++ (printSolidity nextHighLevel)
printSolidity (ImportDir directive nextHighLevel) = (printImportDirective directive) ++ (printSolidity nextHighLevel)
printSolidity (ContractDef definition nextHighLevel) = (printContractDefinition definition) ++ (printSolidity nextHighLevel)
printSolidity (InterfaceDef definition nextHighLevel) = (printInterfaceDefinition definition) ++ (printSolidity nextHighLevel)
printSolidity (LibraryDef definition nextHighLevel) = (printLibraryDefinition definition) ++ (printSolidity nextHighLevel)
printSolidity (FunctionDef definition nextHighLevel) = (printFunctionDefinition definition) ++ (printSolidity nextHighLevel)
printSolidity (ConstVariableDec declaration nextHighLevel) = (printConstVariableDeclaration declaration) ++ (printSolidity nextHighLevel)
printSolidity (StructDef struct nextHighLevel) = (printStructDefinition struct) ++ (printSolidity nextHighLevel)
printSolidity (EnumDef definition nextHighLevel) = (printEnumDefinition definition) ++ (printSolidity nextHighLevel)

printPragmaToken :: PragmaToken -> String
printPragmaToken (PragmaToken x) = [x]

printPragmaTokenList :: [PragmaToken] -> String
printPragmaTokenList [] = ""
printPragmaTokenList (x:xs) = printPragmaToken x ++ printPragmaTokenList xs

printImportDirective :: ImportDirective -> String
printImportDirective (ImportPath path Nothing) = "import " ++ (printPath path) ++ " ;\n"
printImportDirective (ImportPath path (Just x)) = "import " ++ (printPath path) ++ " as " ++ (printIdentifier x) ++ ";"
printImportDirective (ImportSymbolAlisases symbolAliases path) = "import " ++ (printSymbolAliases symbolAliases) ++ " from " ++ (printPath path) ++ ";"
printImportDirective (ImportStar identifier path) = "import * as " ++ (printIdentifier identifier) ++ " from " ++ (printPath path) ++ ";"

printContractDefinition :: ContractDefinition -> String
printContractDefinition (ContractDefinition (Nothing) identifier (Nothing) contractBodyElems) = "contract " ++ (printIdentifier identifier) ++ " {\n" ++ (printContractBodyElementList contractBodyElems 1) ++ "}\n"
printContractDefinition (ContractDefinition (Just x) identifier (Just y) contractBodyElems) = "abstract contract " ++ (printIdentifier identifier) ++ " is " ++ (printInheritanceSpecifierList y) ++ " {\n" ++ (printContractBodyElementList contractBodyElems 1) ++ "}\n"
printContractDefinition (ContractDefinition (Nothing) identifier (Just y) contractBodyElems) = "contract " ++ (printIdentifier identifier) ++ " is " ++ (printInheritanceSpecifierList y) ++ " {\n" ++ (printContractBodyElementList contractBodyElems 1) ++ "}\n"
printContractDefinition (ContractDefinition (Just x) identifier (Nothing) contractBodyElems) = "abstract contract " ++ (printIdentifier identifier) ++ " {\n" ++ (printContractBodyElementList contractBodyElems 1) ++ "}\n"

printContractBodyElement :: ContractBodyElement -> String
printContractBodyElement (ConstructElem constructorDefinition) = (printConstructorDefinition constructorDefinition)
printContractBodyElement (FunctionElem functionDefinition) = (printFunctionDefinition functionDefinition)
printContractBodyElement (ModifierElem modifierDefinition) = (printModifierDefinition modifierDefinition)
printContractBodyElement (FallbackFunctionElem fallbackFunctionDefinition) = (printFallbackFunctionDefintion fallbackFunctionDefinition)
printContractBodyElement (ReceiveFunctionElem receiveFunctionDefinition) = (printReceiveFunctionDefinition receiveFunctionDefinition)
printContractBodyElement (StructElem structDefinition) = (printStructDefinition structDefinition)
printContractBodyElement (EnumElem enumDefinition) = (printEnumDefinition enumDefinition)
printContractBodyElement (StateVariableElem stateVariableDeclaration) = (printStateVariableDeclaration stateVariableDeclaration)
printContractBodyElement (EventElem eventDefinition) = (printEventDefinition eventDefinition)
printContractBodyElement (UsingDirectiveElem usingDirective) = (printUsingDirective usingDirective)

printContractBodyElementList :: [ContractBodyElement] -> Int -> String
printContractBodyElementList [] _ = ""
printContractBodyElementList (x:xs) tabCount = (duplicate "\t" tabCount) ++ (printContractBodyElement x) ++ (printContractBodyElementList xs tabCount)

printInheritanceSpecifierList :: [InheritanceSpecifier] -> String
printInheritanceSpecifierList [] = ""
printInheritanceSpecifierList [x] = printInheritanceSpecifier x
printInheritanceSpecifierList (x:xs) = printInheritanceSpecifier x ++ ", " ++ printInheritanceSpecifierList xs

printInterfaceDefinition :: InterfaceDefinition -> String
printInterfaceDefinition (InterfaceDefinition identifer (Nothing) contractBodyElems) = "interface " ++ (printIdentifier identifer) ++ " {\n" ++ (printContractBodyElementList contractBodyElems 1) ++ "}\n"
printInterfaceDefinition (InterfaceDefinition identifer (Just x) contractBodyElems) = "interface " ++ (printIdentifier identifer) ++ " is " ++ (printInheritanceSpecifierList x) ++ " {\n" ++ (printContractBodyElementList contractBodyElems 1) ++ "}\n"

printLibraryDefinition :: LibraryDefinition -> String
printLibraryDefinition (LibraryDefinition identifier contractBodyElems) = "library " ++ (printIdentifier identifier) ++ " {\n" ++ (printContractBodyElementList contractBodyElems 1) ++ "}\n"

printPath :: Path -> String
printPath (DoubleQuotedPath litDouble) = "\"" ++ (printStringLitDouble litDouble) ++ "\""
printPath (SingleQuotedPath litSingle) = "\'" ++ (printStringLitSingle litSingle) ++ "\'"

printFunctionDefinition :: FunctionDefinition -> String
printFunctionDefinition (FunctionaDefinition funcName (Nothing) funcModifiers (Nothing) (Nothing)) = "function " ++ (printFunctionName funcName) ++ "()" ++ (printFunctionModifiers funcModifiers) ++ ";"
printFunctionDefinition (FunctionaDefinition funcName (Just x) funcModifiers (Nothing) (Just y)) = "function " ++ (printFunctionName funcName) ++ "(" ++ (printParameterList x) ++ ")" ++ (printFunctionModifiers funcModifiers) ++ (printBlock y)
printFunctionDefinition (FunctionaDefinition funcName (Just x) funcModifiers (Nothing) (Nothing)) = "function " ++ (printFunctionName funcName) ++ "(" ++ (printParameterList x) ++ ")" ++ (printFunctionModifiers funcModifiers) ++ ";"
printFunctionDefinition (FunctionaDefinition funcName (Nothing) funcModifiers (Nothing) (Just y)) = "function " ++ (printFunctionName funcName) ++ "()" ++ (printFunctionModifiers funcModifiers) ++ (printBlock y)

printFunctionDefinition (FunctionaDefinition funcName (Nothing) funcModifiers (Just z) (Nothing)) = "function " ++ (printFunctionName funcName) ++ "()" ++ (printFunctionModifiers funcModifiers) ++ "returns (" ++ (printParameterList z) ++ ")" ++ ";"
printFunctionDefinition (FunctionaDefinition funcName (Just x) funcModifiers (Just z) (Just y)) = "function " ++ (printFunctionName funcName) ++ "(" ++ (printParameterList x) ++ ")" ++ (printFunctionModifiers funcModifiers) ++ "returns (" ++ (printParameterList z) ++ ")" ++ (printBlock y)
printFunctionDefinition (FunctionaDefinition funcName (Just x) funcModifiers (Just z) (Nothing)) = "function " ++ (printFunctionName funcName) ++ "(" ++ (printParameterList x) ++ ")" ++ (printFunctionModifiers funcModifiers) ++ "returns (" ++ (printParameterList z) ++ ")" ++ ";"
printFunctionDefinition (FunctionaDefinition funcName (Nothing) funcModifiers (Just z) (Just y)) = "function " ++ (printFunctionName funcName) ++ "()" ++ (printFunctionModifiers funcModifiers) ++ "returns (" ++ (printParameterList z) ++ ")" ++ (printBlock y)

printFunctionModifier :: FunctionModifiers -> String
printFunctionModifier (VisibilityModifier visibility) = (printVisibility visibility) ++ " "
printFunctionModifier (StateMutabilityModifier stateMutability) = (printStateMutability stateMutability) ++ " "
printFunctionModifier (ModifierInvoc modifierInvocation) = (printModifierInvocation modifierInvocation) ++ " "
printFunctionModifier (Virtual) = "virtual "
printFunctionModifier (OverrideMod overrideSpecifier) = (printOverrideSpecifier overrideSpecifier) ++ " "

printFunctionModifiers :: [FunctionModifiers] -> String
printFunctionModifiers [] = ""
printFunctionModifiers (x:xs) = (printFunctionModifier x) ++ (printFunctionModifiers xs)

printSymbolAliases :: SymbolAliases -> String
printSymbolAliases (IdentifierAlias identifier) = "{" ++ (printIdentifier identifier) ++ "}"
printSymbolAliases (As identifier1 identifier2) = "{" ++ (printIdentifier identifier1) ++ " as " ++ (printIdentifier identifier2) ++ "}"
printSymbolAliases (List symbolAliases) = "{" ++ (printSymbolAliasesList symbolAliases) ++ "}"

printSymbolAliasesList :: [SymbolAliases] -> String
printSymbolAliasesList [(IdentifierAlias identifier)] = (printIdentifier identifier)
printSymbolAliasesList [(As identifier1 identifier2)] = (printIdentifier identifier1) ++ " as " ++ (printIdentifier identifier2)
printSymbolAliasesList ((IdentifierAlias identifier):xs) = (printIdentifier identifier) ++ ", " ++ (printSymbolAliasesList xs)
printSymbolAliasesList ((As identifier1 identifier2):xs) = (printIdentifier identifier1) ++ " as " ++ (printIdentifier identifier2) ++ ", " ++ (printSymbolAliasesList xs)

printIdentifier :: Identifier -> String
printIdentifier (From) = "from"
printIdentifier (Char x xs) = [x] ++ xs

printOverrideSpecifier :: OverrideSpecifier -> String
printOverrideSpecifier (OverrideSpecifier Nothing) = "override"
printOverrideSpecifier (OverrideSpecifier (Just identifierPathList)) = "override (" ++ (printIdentifierPathList identifierPathList) ++ ")"

printIdentifierPath :: IdentifierPath -> String
printIdentifierPath (IdentifierPath identifier []) = (printIdentifier identifier)
printIdentifierPath (IdentifierPath identifier (x:xs)) = (printIdentifier identifier) ++ "." ++ (printIdentifierPath (IdentifierPath x xs))

printIdentifierPathList :: [IdentifierPath] -> String
printIdentifierPathList [x] = printIdentifierPath x
printIdentifierPathList (x:xs) = (printIdentifierPath x) ++ ", " ++ (printIdentifierPathList xs)

printModifierInvocation :: ModifierInvocation -> String
printModifierInvocation (ModifierInvocation identifierPath (Just callArgumentList)) = (printIdentifierPath identifierPath) ++ " " ++ (printCallArgumentList callArgumentList)
printModifierInvocation (ModifierInvocation identifierPath (Nothing)) = (printIdentifierPath identifierPath)

printFunctionName :: FunctionName -> String
printFunctionName (IdentifierName identifier) = printIdentifier identifier
printFunctionName Fallback = "fallback"
printFunctionName Receive = "receive"

printVisibility :: Visibility -> String
printVisibility InternalVisibility = "internal"
printVisibility ExternalVisibility = "external"
printVisibility PrivateVisibility = "private"
printVisibility PublicVisibility = "public"

printParameterList :: ParameterList -> String
printParameterList (ParameterList typeName (Nothing) (Nothing) []) = (printTypeName typeName)
printParameterList (ParameterList typeName (Just x) (Just y) []) = (printTypeName typeName) ++ " " ++ (printDataLocation x) ++ " " ++ (printIdentifier y)
printParameterList (ParameterList typeName (Just x) (Nothing) []) = (printTypeName typeName) ++ " " ++ (printDataLocation x)
printParameterList (ParameterList typeName (Nothing) (Just y) []) = (printTypeName typeName) ++ " " ++ (printIdentifier y)
printParameterList (ParameterList typeName (Nothing) (Nothing) (xs)) = (printTypeName typeName) ++ ", " ++ (printListParameterList xs)
printParameterList (ParameterList typeName (Just x) (Just y) (xs)) = (printTypeName typeName) ++ " " ++ (printDataLocation x) ++ " " ++ (printIdentifier y) ++ ", " ++ (printListParameterList xs)
printParameterList (ParameterList typeName (Just x) (Nothing) (xs)) = (printTypeName typeName) ++ " " ++ (printDataLocation x) ++ ", " ++ (printListParameterList xs)
printParameterList (ParameterList typeName (Nothing) (Just y) (xs)) = (printTypeName typeName) ++ " " ++ (printIdentifier y) ++ ", " ++ (printListParameterList xs)

printListParameterList :: [ParameterList] -> String
printListParameterList [x] = printParameterList x
printListParameterList (x:xs) = (printParameterList x) ++ (printListParameterList xs)

printDataLocation :: DataLocation -> String
printDataLocation Memory = "memory"
printDataLocation Storage = "storage"
printDataLocation CallData = "calldata"

printTypeName :: TypeName -> String
printTypeName (ElementaryType elementaryTypeName) = printElementaryTypeName elementaryTypeName
printTypeName (FunctionType functionTypeName) = printFunctionTypeName functionTypeName
printTypeName (Mapping mappingType) = printMappingType mappingType
printTypeName (IdentifierPathType identifierPath) = printIdentifierPath identifierPath
printTypeName (TypeNameExpression typeName (Just expression)) = printTypeName typeName ++ "[" ++ (printExpression expression) ++ "]"
printTypeName (TypeNameExpression typeName (Nothing)) = printTypeName typeName ++ "[]"

printElementaryTypeName :: ElementaryTypeName -> String
printElementaryTypeName (AddressType (Just x)) = "address payable"
printElementaryTypeName (AddressType (Nothing)) = "address"
printElementaryTypeName Bool = "bool"
printElementaryTypeName String = "string"
printElementaryTypeName Bytes = "bytes"
printElementaryTypeName (SignedIntType signedInt) = printSignedInt signedInt
printElementaryTypeName (UnsignedIntType unsignedInt) = printUnsignedInt unsignedInt
printElementaryTypeName (FixedBytesType fixedBytes) = printFixedBytes fixedBytes
printElementaryTypeName Fixed = "fixed"
printElementaryTypeName UFixed = "ufixed"

printSignedInt :: SignedInt -> String
printSignedInt Int = "int"
printSignedInt Int8 = "int8"
printSignedInt Int16 = "int16"
printSignedInt Int24 = "int24"
printSignedInt Int32 = "int32"
printSignedInt Int40 = "int40"
printSignedInt Int48 = "int48"
printSignedInt Int56 = "int56"
printSignedInt Int64 = "int64"
printSignedInt Int72 = "int72"
printSignedInt Int80 = "int80"
printSignedInt Int88 = "int88"
printSignedInt Int96 = "int96"
printSignedInt Int104 = "int104"
printSignedInt Int112 = "int112"
printSignedInt Int120 = "int120"
printSignedInt Int128 = "int128"
printSignedInt Int136 = "int136"
printSignedInt Int144 = "int144"
printSignedInt Int152 = "int152"
printSignedInt Int160 = "int160"
printSignedInt Int168 = "int168"
printSignedInt Int176 = "int176"
printSignedInt Int184 = "int184"
printSignedInt Int192 = "int192"
printSignedInt Int200 = "int200"
printSignedInt Int208 = "int208"
printSignedInt Int216 = "int216"
printSignedInt Int224 = "int224"
printSignedInt Int232 = "int232"
printSignedInt Int240 = "int240"
printSignedInt Int248 = "int248"
printSignedInt Int256 = "int256"

printUnsignedInt :: UnsignedInt -> String
printUnsignedInt UInt = "uint"
printUnsignedInt UInt8 = "uint8"
printUnsignedInt UInt16 = "uint16"
printUnsignedInt UInt24 = "uint24"
printUnsignedInt UInt32 = "uint32"
printUnsignedInt UInt40 = "uint40"
printUnsignedInt UInt48 = "uint48"
printUnsignedInt UInt56 = "uint56"
printUnsignedInt UInt64 = "uint64"
printUnsignedInt UInt72 = "uint72"
printUnsignedInt UInt80 = "uint80"
printUnsignedInt UInt88 = "uint88"
printUnsignedInt UInt96 = "uint96"
printUnsignedInt UInt104 = "uint104"
printUnsignedInt UInt112 = "uint112"
printUnsignedInt UInt120 = "uint120"
printUnsignedInt UInt128 = "uint128"
printUnsignedInt UInt136 = "uint136"
printUnsignedInt UInt144 = "uint144"
printUnsignedInt UInt152 = "uint152"
printUnsignedInt UInt160 = "uint160"
printUnsignedInt UInt168 = "uint168"
printUnsignedInt UInt176 = "uint176"
printUnsignedInt UInt184 = "uint184"
printUnsignedInt UInt192 = "uint192"
printUnsignedInt UInt200 = "uint200"
printUnsignedInt UInt208 = "uint208"
printUnsignedInt UInt216 = "uint216"
printUnsignedInt UInt224 = "uint224"
printUnsignedInt UInt232 = "uint232"
printUnsignedInt UInt240 = "uint240"
printUnsignedInt UInt248 = "uint248"
printUnsignedInt UInt256 = "uint256"

printFixedBytes :: FixedBytes -> String
printFixedBytes Bytes1 = "bytes1"
printFixedBytes Bytes2 = "bytes2"
printFixedBytes Bytes3 = "bytes3"
printFixedBytes Bytes4 = "bytes4"
printFixedBytes Bytes5 = "bytes5"
printFixedBytes Bytes6 = "bytes6"
printFixedBytes Bytes7 = "bytes7"
printFixedBytes Bytes8 = "bytes8"
printFixedBytes Bytes9 = "bytes9"
printFixedBytes Bytes10 = "bytes10"
printFixedBytes Bytes11 = "bytes11"
printFixedBytes Bytes12 = "bytes12"
printFixedBytes Bytes13 = "bytes13"
printFixedBytes Bytes14 = "bytes14"
printFixedBytes Bytes15 = "bytes15"
printFixedBytes Bytes16 = "bytes16"
printFixedBytes Bytes17 = "bytes17"
printFixedBytes Bytes18 = "bytes18"
printFixedBytes Bytes19 = "bytes19"
printFixedBytes Bytes20 = "bytes20"
printFixedBytes Bytes21 = "bytes21"
printFixedBytes Bytes22 = "bytes22"
printFixedBytes Bytes23 = "bytes23"
printFixedBytes Bytes24 = "bytes24"
printFixedBytes Bytes25 = "bytes25"
printFixedBytes Bytes26 = "bytes26"
printFixedBytes Bytes27 = "bytes27"
printFixedBytes Bytes28 = "bytes28"
printFixedBytes Bytes29 = "bytes29"
printFixedBytes Bytes30 = "bytes30"
printFixedBytes Bytes31 = "bytes31"
printFixedBytes Bytes32 = "bytes32"

printMappingType :: MappingType -> String
printMappingType (MappingType mappingKeyType typeName) = "mapping ()" ++ (printMappingKeyType mappingKeyType) ++ " => " ++ (printTypeName typeName) ++ ")"

printMappingKeyType :: MappingKeyType -> String
printMappingKeyType (ElementaryMapping elementaryTypeName) = (printElementaryTypeName elementaryTypeName)
printMappingKeyType (IdentifierMapping identifierPath) = (printIdentifierPath identifierPath)

printFunctionTypeName :: FunctionTypeName -> String
printFunctionTypeName (FunctionTypeName (Nothing) (stateMutability1) (Nothing) (stateMutability2) (Nothing)) = "function () " ++ (printStateMutabilityList stateMutability1) ++ (printStateMutabilityList stateMutability1)
printFunctionTypeName (FunctionTypeName (Nothing) (stateMutability1) (Nothing) (stateMutability2) (Just z)) = "function () " ++ (printStateMutabilityList stateMutability1) ++ (printStateMutabilityList stateMutability1) ++ " returns (" ++ (printParameterList z) ++ ")"
printFunctionTypeName (FunctionTypeName (Nothing) (stateMutability1) (Just y) (stateMutability2) (Nothing)) = "function () " ++ (printStateMutabilityList stateMutability1) ++ (printVisibility y) ++ (printStateMutabilityList stateMutability1)
printFunctionTypeName (FunctionTypeName (Nothing) (stateMutability1) (Just y) (stateMutability2) (Just z)) = "function () " ++ (printStateMutabilityList stateMutability1) ++ (printVisibility y) ++ (printStateMutabilityList stateMutability1) ++ " returns (" ++ (printParameterList z) ++ ")"
printFunctionTypeName (FunctionTypeName (Just x) (stateMutability1) (Nothing) (stateMutability2) (Nothing)) = "function (" ++ (printParameterList x) ++ ") " ++ (printStateMutabilityList stateMutability1) ++ (printStateMutabilityList stateMutability1)
printFunctionTypeName (FunctionTypeName (Just x) (stateMutability1) (Nothing) (stateMutability2) (Just z)) = "function (" ++ (printParameterList x) ++ ") " ++ (printStateMutabilityList stateMutability1) ++ (printStateMutabilityList stateMutability1) ++ " returns (" ++ (printParameterList z) ++ ")"
printFunctionTypeName (FunctionTypeName (Just x) (stateMutability1) (Just y) (stateMutability2) (Nothing)) = "function (" ++ (printParameterList x) ++ ") " ++ (printStateMutabilityList stateMutability1) ++ (printVisibility y) ++ (printStateMutabilityList stateMutability1)
printFunctionTypeName (FunctionTypeName (Just x) (stateMutability1) (Just y) (stateMutability2) (Just z)) = "function (" ++ (printParameterList x) ++ ") " ++ (printStateMutabilityList stateMutability1) ++ (printVisibility y) ++ (printStateMutabilityList stateMutability1) ++ " returns (" ++ (printParameterList z) ++ ")"

printStateMutability :: StateMutability -> String
printStateMutability Pure = "pure"
printStateMutability View = "view"
printStateMutability PayableMutability = "payable"

printStateMutabilityList :: [StateMutability] -> String
printStateMutabilityList [] = ""
printStateMutabilityList (x:xs) = (printStateMutability x) ++ (printStateMutabilityList xs)

printStringLitDouble :: StringLitDouble -> String
printStringLitDouble (DoubleQuotePrintable doubleQuotePrintable) = [doubleQuotePrintable]
printStringLitDouble (Double escapeSequence) = printEscapeSequence escapeSequence

printStringLitSingle :: StringLitSingle -> String
printStringLitSingle (SingleQuotedPrintable singleQuotedPrintable) = [singleQuotedPrintable]
printStringLitSingle (Single escapeSequence) = printEscapeSequence escapeSequence

printEscapeSequence :: EscapeSequence -> String
printEscapeSequence (U c1 c2 c3 c4) = "\\u" ++ [c1] ++ [c2] ++ [c3] ++ [c4]
printEscapeSequence (X c1 c2) = "\\x" ++ [c1] ++ [c2]
printEscapeSequence (Simple char) = "\\" ++ [char]

printEscapeSequenceList :: [EscapeSequence] -> String
printEscapeSequenceList [] = ""
printEscapeSequenceList (x:xs) = (printEscapeSequence x) ++ (printEscapeSequenceList xs)

printBlock :: Block -> String
printBlock (Block (Nothing) (Nothing) (Nothing)) = "{}"
printBlock (Block (Nothing) (Nothing) (Just unCheckedBlock)) = "{" ++ (printUncheckedBlock unCheckedBlock) ++ "}"
printBlock (Block (Nothing) (Just statement) (Nothing)) = "{" ++ (printStatement statement) ++ "}"
printBlock (Block (Nothing) (Just statement) (Just unCheckedBlock)) =  "{" ++ (printStatement statement) ++ (printUncheckedBlock unCheckedBlock) ++ "}"
printBlock (Block (Just unCheckedBlock) (Nothing) (Nothing)) = "{" ++ (printUncheckedBlock unCheckedBlock) ++ "}"
printBlock (Block (Just unCheckedBlock1) (Nothing) (Just unCheckedBlock2)) = "{" ++ (printUncheckedBlock unCheckedBlock1) ++ (printUncheckedBlock unCheckedBlock1) ++ "}"
printBlock (Block (Just unCheckedBlock) (Just statement) (Nothing)) = "{" ++ (printUncheckedBlock unCheckedBlock) ++ (printStatement statement)  ++ "}"
printBlock (Block (Just unCheckedBlock1) (Just statement) (Just unCheckedBlock2)) =  "{" ++ (printUncheckedBlock unCheckedBlock1) ++ (printStatement statement)  ++ (printUncheckedBlock unCheckedBlock2) ++ "}"

printUncheckedBlock :: UnCheckedBlock -> String
printUncheckedBlock (UnCheckedBlock block) = "unchecked " ++ (printBlock block)

printStructDefinition :: StructDefinition -> String
printStructDefinition (StructDefinition identifier structMember structMemberList) = "struct " ++ (printIdentifier identifier) ++ " {" ++ (printStructMember structMember) ++ (printStructMemberList structMemberList) ++ "}"

printStructMember :: StructMember -> String
printStructMember (StructMember typeName identifier) = (printTypeName typeName) ++ " " ++ (printIdentifier identifier) ++ ";"

printStructMemberList :: [StructMember] -> String
printStructMemberList [] = ""
printStructMemberList (x:xs) = (printStructMember x) ++ (printStructMemberList xs)

printConstVariableDeclaration :: ConstantVariableDeclaration -> String
printConstVariableDeclaration (ConstantVariableDeclaration typeName identifier expression) = (printTypeName typeName) ++ " constant " ++ (printIdentifier identifier) ++ " = " ++ (printExpression expression) ++ ";"

printEnumDefinition :: EnumDefinition -> String
printEnumDefinition (EnumDefinition identifier firstIdentifier []) = "enum " ++ (printIdentifier identifier) ++ " { " ++ (printIdentifier firstIdentifier) ++ "}"
printEnumDefinition (EnumDefinition identifier firstIdentifier remainingIdentifiers) = "enum " ++ (printIdentifier identifier) ++ " { " ++ (printIdentifier firstIdentifier) ++ ", " ++ (printCommaSpacedIdentifiers remainingIdentifiers) ++ "}"

printCommaSpacedIdentifiers :: [Identifier] -> String
printCommaSpacedIdentifiers [x] = printIdentifier x
printCommaSpacedIdentifiers (x:xs) = (printIdentifier x) ++ ", " ++ (printCommaSpacedIdentifiers xs)

printExpression :: Expression -> String
printExpression (Index expression1 expression2) = (printExpression expression1) ++ "[" ++ (printExpression expression2) ++ "]"
printExpression (IndexOfIndex expression1 ((Just expression2),(Just expression3))) = (printExpression expression1) ++ "[" ++ (printExpression expression2) ++ " : " ++ (printExpression expression3) ++ "]"
printExpression (IndexOfIndex expression1 ((Just expression2),(Nothing))) = (printExpression expression1) ++ "[" ++ (printExpression expression2) ++ " : " ++ "]"
printExpression (IndexOfIndex expression1 ((Nothing),(Just expression3))) = (printExpression expression1) ++ "[" ++ " : " ++ (printExpression expression3) ++ "]"
printExpression (IndexOfIndex expression1 ((Nothing),(Nothing))) = (printExpression expression1) ++ "[" ++ " : " ++ "]"
printExpression (DotIdentifier expression identifier) = (printExpression expression) ++ "." ++ (printIdentifier identifier)
printExpression (DotAddress expression) = (printExpression expression) ++ ".address"
printExpression (IdentifierExpression expression identifierExpressionPairs) = (printExpression expression) ++ "{" ++ (printIdentifierExpressionPairs identifierExpressionPairs) ++ "}"
printExpression (ExpressionArgs expression callArgumentList) = (printExpression expression) ++ (printCallArgumentList callArgumentList)
printExpression (Payable callArgumentList) = "payable" ++ (printCallArgumentList callArgumentList)
printExpression (Type typeName) = "type" ++ "(" ++ (printTypeName typeName) ++ ")"
printExpression (PreIncrement expression) = "++" ++ (printExpression expression)
printExpression (PreDecrement expression) = "--" ++ (printExpression expression)
printExpression (Not expression) = "!" ++ (printExpression expression)
printExpression (Tilda expression) = "~" ++ (printExpression expression)
printExpression (Delete expression) = "delete" ++ (printExpression expression)
printExpression (Minus expression) = "-" ++ (printExpression expression)
printExpression (PostIncrement expression) = (printExpression expression) ++ "++"
printExpression (PostDecrement expression) = (printExpression expression) ++ "--"
printExpression (Pow expression1 expression2) = (printExpression expression1) ++ " ** " ++ (printExpression expression2)
printExpression (Mul expression1 expression2) = (printExpression expression1) ++ " * " ++ (printExpression expression2)
printExpression (Div expression1 expression2) = (printExpression expression1) ++ " / " ++ (printExpression expression2)
printExpression (Mod expression1 expression2) = (printExpression expression1) ++ " % " ++ (printExpression expression2)
printExpression (Add expression1 expression2) = (printExpression expression1) ++ " + " ++ (printExpression expression2)
printExpression (Sub expression1 expression2) = (printExpression expression1) ++ " - " ++ (printExpression expression2)
printExpression (ArithmeticLeftShift expression1 expression2) = (printExpression expression1) ++ " << " ++ (printExpression expression2)
printExpression (ArithmeticRightShift expression1 expression2) = (printExpression expression1) ++ " >> " ++ (printExpression expression2)
printExpression (LogicalRightShift expression1 expression2) = (printExpression expression1) ++ " >>> " ++ (printExpression expression2)
printExpression (BitAND expression1 expression2) = (printExpression expression1) ++ " & " ++ (printExpression expression2)
printExpression (Caret expression1 expression2) = (printExpression expression1) ++ " ^ " ++ (printExpression expression2)
printExpression (BitOR expression1 expression2) = (printExpression expression1) ++ " | " ++ (printExpression expression2)
printExpression (LessThan expression1 expression2) = (printExpression expression1) ++ " < " ++ (printExpression expression2)
printExpression (GreaterThan expression1 expression2) = (printExpression expression1) ++ " > " ++ (printExpression expression2)
printExpression (LessthanEqualTo expression1 expression2) = (printExpression expression1) ++ " <= " ++ (printExpression expression2)
printExpression (GreaterThanEQualTo expression1 expression2) = (printExpression expression1) ++ " >= " ++ (printExpression expression2)
printExpression (Equality expression1 expression2) = (printExpression expression1) ++ " == " ++ (printExpression expression2)
printExpression (InEquality expression1 expression2) = (printExpression expression1) ++ " != " ++ (printExpression expression2)
printExpression (AND expression1 expression2) = (printExpression expression1) ++ " && " ++ (printExpression expression2)
printExpression (OR expression1 expression2) = (printExpression expression1) ++ " || " ++ (printExpression expression2)
printExpression (InlineIf expression1 expression2 expression3) = (printExpression expression1) ++ " ? " ++ (printExpression expression2) ++ " : " ++ (printExpression expression3)
printExpression (Equals expression1 expression2) = (printExpression expression1) ++ " = " ++ (printExpression expression2)
printExpression (OrEquals expression1 expression2) = (printExpression expression1) ++ " |= " ++ (printExpression expression2)
printExpression (CaretEquals expression1 expression2) = (printExpression expression1) ++ " ^= " ++ (printExpression expression2)
printExpression (AndEquals expression1 expression2) = (printExpression expression1) ++ " &= " ++ (printExpression expression2)
printExpression (LeftShiftEquals expression1 expression2) = (printExpression expression1) ++ " <<= " ++ (printExpression expression2)
printExpression (RightShiftEquals expression1 expression2) = (printExpression expression1) ++ " >>= " ++ (printExpression expression2)
printExpression (LogicalRightShiftEquals expression1 expression2) = (printExpression expression1) ++ " >>>= " ++ (printExpression expression2)
printExpression (PlusEquals expression1 expression2) = (printExpression expression1) ++ " += " ++ (printExpression expression2)
printExpression (MinusEquals expression1 expression2) = (printExpression expression1) ++ " -= " ++ (printExpression expression2)
printExpression (MulEquals expression1 expression2) = (printExpression expression1) ++ " *= " ++ (printExpression expression2)
printExpression (DivEquals expression1 expression2) = (printExpression expression1) ++ " /= " ++ (printExpression expression2)
printExpression (ModEquals expression1 expression2) = (printExpression expression1) ++ " %= " ++ (printExpression expression2)
printExpression (New typeName) = "new " ++ (printTypeName typeName)
printExpression (TupleExpr tupleExpression) = (printTupleExpression tupleExpression)
printExpression (InlineArrayExpr inlineArrayExpression) = (printInlineArrayExpression inlineArrayExpression)
printExpression (IdentifierExpr identifier) = (printIdentifier identifier)
printExpression (LiteralExpr literal) = (printLiteral literal)
printExpression (ElementaryTypeNameExpr elementaryTypeName) = (printElementaryTypeName elementaryTypeName)

printIdentifierExpressionPair :: (Identifier, Expression) -> String
printIdentifierExpressionPair (identifier, expression) = (printIdentifier identifier) ++ " : " ++ (printExpression expression)

printIdentifierExpressionPairs :: [(Identifier, Expression)] -> String
printIdentifierExpressionPairs [x] = printIdentifierExpressionPair x
printIdentifierExpressionPairs (x:xs) = (printIdentifierExpressionPair x) ++ ", " ++ (printIdentifierExpressionPairs xs)

printStatement :: Statement -> String
printStatement (BlockStatement block) = (printBlock block)
printStatement (VarDec variableDeclarationStatement) = (printVariableDeclarationStatement variableDeclarationStatement)
printStatement (ExprStatement expressionStatement) = (printExpressionStatement expressionStatement)
printStatement (If ifStatement) = (printIfStatement ifStatement)
printStatement (For forStatement) = (printForStatement forStatement)
printStatement (While whileStatement) = (printWhileStatement whileStatement)
printStatement (DoWhile doWhileStatement) = (printDoWhileStatement doWhileStatement)
printStatement (Continue continueStatement) = "continue;"
printStatement (Break breakStatement) = "break;"
printStatement (Try tryStatement) = (printTryStatement tryStatement)
printStatement (Return returnStatement) = (printReturnStatement returnStatement)
printStatement (Emit emitStatement) = (printEmitStatement emitStatement)
printStatement (Assembly assemblyStatement) = (printAssemblyStatement assemblyStatement)

printVariableDeclarationStatement :: VariableDeclarationStatement -> String
printVariableDeclarationStatement (SingleVariableDeclartion variableDeclaration expression) = (printVariableDeclaration variableDeclaration) ++ " = " ++ (printExpression expression) ++ ";"
printVariableDeclarationStatement (TupleVariableDeclaration variableDeclarationTuple expression) = (printVariableDeclarationTuple variableDeclarationTuple) ++ " = " ++ (printExpression expression) ++ ";"

printReturnStatement :: ReturnStatement -> String
printReturnStatement (ReturnStatement (Just expression)) = "return " ++ (printExpression expression) ++ ";"
printReturnStatement (ReturnStatement (Nothing)) = "return;"

printAssemblyStatement :: AssemblyStatement -> String
printAssemblyStatement (AssemblyStatement (Just evmasm) yulStatementList) = "assembly \'\"evasm\"\' { " ++ (printYulStatementList yulStatementList) ++ "}"
printAssemblyStatement (AssemblyStatement (Nothing) yulStatementList) = "assembly { " ++ (printYulStatementList yulStatementList) ++ "}"

printTryStatement :: TryStatement -> String
printTryStatement (TryStatement expression (Just parameterList) block catchClause catchClauseList) = "try " ++ (printExpression expression) ++ " returns (" ++ (printParameterList parameterList) ++ ") " ++ (printBlock block) ++ (printCatchClause catchClause) ++ (printCatchClauseList catchClauseList)
printTryStatement (TryStatement expression (Nothing) block catchClause catchClauseList) = "try " ++ (printExpression expression) ++ (printBlock block) ++ (printCatchClause catchClause) ++ (printCatchClauseList catchClauseList)

printCatchClause :: CatchClause -> String
printCatchClause (CatchClause block) = "catch " ++ (printBlock block)
printCatchClause (CatchClauseParameters (Just identifier) parameterList block) = "catch " ++ (printIdentifier identifier) ++ "(" ++ (printParameterList parameterList) ++ ")" ++ (printBlock block)
printCatchClause (CatchClauseParameters (Nothing) parameterList block) = "catch " ++  "(" ++ (printParameterList parameterList) ++ ")" ++ (printBlock block)

printCatchClauseList :: [CatchClause] -> String
printCatchClauseList [] = ""
printCatchClauseList (x:xs) = (printCatchClause x) ++ (printCatchClauseList xs)

printConstructorDefinition :: ConstructorDefinition -> String
printConstructorDefinition (Constructor (Just parameterList) (Just modifiers) block) = "constructor (" ++ (printParameterList parameterList) ++ ") " ++ (printModifiersList modifiers) ++ (printBlock block)
printConstructorDefinition (Constructor (Just parameterList) (Nothing) block) = "constructor (" ++ (printParameterList parameterList) ++ ") " ++ (printBlock block)
printConstructorDefinition (Constructor (Nothing) (Just modifiers) block) = "constructor () " ++ (printModifiersList modifiers) ++ (printBlock block)
printConstructorDefinition (Constructor (Nothing) (Nothing) block) = "constructor () " ++ (printBlock block)

printModifiers :: Modifiers -> String
printModifiers (Invocation modifierInvocation) = (printModifierInvocation modifierInvocation)
printModifiers PayableModifier = "payable"
printModifiers Internal = "internal"
printModifiers Public = "public"

printModifiersList :: [Modifiers] -> String
printModifiersList [] = ""
printModifiersList [x] = (printModifiers x)
printModifiersList (x:xs) = (printModifiers x) ++ " " ++ (printModifiersList xs)

printIfStatement :: IfStatement -> String
printIfStatement (IfStatement expression statement (Just elseStatement)) = "if (" ++ (printExpression expression) ++ ") " ++ (printStatement statement) ++ "else " ++ (printStatement elseStatement)
printIfStatement (IfStatement expression statement (Nothing)) = "if (" ++ (printExpression expression) ++ ") " ++ (printStatement statement)

printWhileStatement :: WhileStatement -> String
printWhileStatement (WhileStatement expression statement) = "while (" ++ (printExpression expression) ++ ") " ++ (printStatement statement)

printDoWhileStatement :: DoWhileStatement -> String
printDoWhileStatement (DoWhileStatement statement expression) = "do " ++ (printStatement statement) ++ " while (" ++ (printExpression expression) ++ ");"

printEmitStatement :: EmitStatement -> String
printEmitStatement (EmitStatement expression callArgumentList) = "emit " ++ (printExpression expression) ++ (printCallArgumentList callArgumentList) ++ ";"

printForStatement :: ForStatement -> String
printForStatement (ForStatement forInitialiser (Just expressionStatement) (Just expression) statement) = "for (" ++ (printForInitialiser forInitialiser) ++ (printExpressionStatement expressionStatement) ++ (printExpression expression) ++ ") " ++ (printStatement statement)
printForStatement (ForStatement forInitialiser (Nothing) (Just expression) statement) = "for (" ++ (printForInitialiser forInitialiser) ++ ("; ") ++ (printExpression expression) ++ ") " ++ (printStatement statement)
printForStatement (ForStatement forInitialiser (Just expressionStatement) (Nothing) statement) = "for (" ++ (printForInitialiser forInitialiser) ++ (printExpressionStatement expressionStatement) ++ ") " ++ (printStatement statement)
printForStatement (ForStatement forInitialiser (Nothing) (Nothing) statement) = "for (" ++ (printForInitialiser forInitialiser) ++ ("; ") ++ ") " ++ (printStatement statement)

printForInitialiser :: ForInitialiser -> String
printForInitialiser (VarInitialse variableDeclarationStatement) = (printVariableDeclarationStatement variableDeclarationStatement)
printForInitialiser (ExprInitialse expressionStatement) = (printExpressionStatement expressionStatement)
printForInitialiser (None) = "; "

printExpressionStatement :: ExpressionStatement -> String
printExpressionStatement (ExpressionStatement expression) = (printExpression expression) ++ "; "

printModifierDefinition :: ModifierDefinition -> String
printModifierDefinition (ModifierDefinition identifier (Just parameterList) (Just overrideSpecifier) (Just block)) = "modifier " ++ (printIdentifier identifier) ++ "(" ++ (printParameterList parameterList) ++ ") " ++ (printOverrideSpecifier overrideSpecifier) ++ "virtual" ++ (printBlock block)
printModifierDefinition (ModifierDefinition identifier (Nothing) (Just overrideSpecifier) (Just block)) = "modifier " ++ (printIdentifier identifier) ++ (printBlock block)
printModifierDefinition (ModifierDefinition identifier (Just parameterList) (Nothing) (Just block)) = "modifier " ++ (printIdentifier identifier) ++ "(" ++ (printParameterList parameterList) ++ ") " ++ (printBlock block)
printModifierDefinition (ModifierDefinition identifier (Nothing) (Nothing) (Just block)) = "modifier " ++ (printIdentifier identifier) ++ (printBlock block)
printModifierDefinition (ModifierDefinition identifier (Just parameterList) (Just overrideSpecifier) (Nothing)) = "modifier " ++ (printIdentifier identifier) ++ "(" ++ (printParameterList parameterList) ++ ") " ++ (printOverrideSpecifier overrideSpecifier) ++ "virtual" ++ (";")
printModifierDefinition (ModifierDefinition identifier (Nothing) (Just overrideSpecifier) (Nothing)) = "modifier " ++ (printIdentifier identifier) ++ (";")
printModifierDefinition (ModifierDefinition identifier (Just parameterList) (Nothing) (Nothing)) = "modifier " ++ (printIdentifier identifier) ++ "(" ++ (printParameterList parameterList) ++ ") " ++ (";")
printModifierDefinition (ModifierDefinition identifier (Nothing) (Nothing) (Nothing)) = "modifier " ++ (printIdentifier identifier) ++ (";")

printFallbackFunctionDefintion :: FallbackFunctionDefinition -> String
printFallbackFunctionDefintion (FallbackFunctionDefinition parameterList fallBackModifiers (Just returnParameterList) (Just block)) = "fallback (" ++ (printParameterList parameterList) ++ ") " ++ (printFallBackModifiers fallBackModifiers) ++ "returns (" ++ (printParameterList returnParameterList) ++ ") " ++ (printBlock block)
printFallbackFunctionDefintion (FallbackFunctionDefinition parameterList fallBackModifiers (Nothing) (Just block)) = "fallback (" ++ (printParameterList parameterList) ++ ") " ++ (printFallBackModifiers fallBackModifiers) ++ (printBlock block)
printFallbackFunctionDefintion (FallbackFunctionDefinition parameterList fallBackModifiers (Just returnParameterList) (Nothing)) = "fallback (" ++ (printParameterList parameterList) ++ ") " ++ (printFallBackModifiers fallBackModifiers) ++ "returns (" ++ (printParameterList returnParameterList) ++ ") " ++ "; "
printFallbackFunctionDefintion (FallbackFunctionDefinition parameterList fallBackModifiers (Nothing) (Nothing)) = "fallback (" ++ (printParameterList parameterList) ++ ") " ++ (printFallBackModifiers fallBackModifiers) ++ "; "

printFallBackModifier :: FallBackModifier -> String
printFallBackModifier (ExternalFallBack) = "external"
printFallBackModifier (StateMutabilityFallBack stateMutability) = (printStateMutability stateMutability)
printFallBackModifier (ModifierInvocFallBack modifierInvocation) = (printModifierInvocation modifierInvocation)
printFallBackModifier (VirtualFallBack) = "virutal"
printFallBackModifier (OverrideSpecFallBack overrideSpecifier) = (printOverrideSpecifier overrideSpecifier)

printFallBackModifiers :: [FallBackModifier] -> String
printFallBackModifiers [] = ""
printFallBackModifiers [x] = (printFallBackModifier x)
printFallBackModifiers (x:xs) = (printFallBackModifier x) ++ " " ++ (printFallBackModifiers xs)

printCommaExpressions :: [Expression] -> String
printCommaExpressions [] = ""
printCommaExpressions [x] = (printExpression x)
printCommaExpressions (x:xs) = (printExpression x) ++ ", " ++ (printCommaExpressions xs)

printCallArgumentList :: CallArgumentList -> String
printCallArgumentList (CallArgumentExpr expression []) = "(" ++ (printExpression expression) ++ ")"
printCallArgumentList (CallArgumentExpr expression expressions) = "(" ++ (printExpression expression) ++ ", " ++ (printCommaExpressions expressions) ++ ")"
printCallArgumentList (CallArgumentExprIdent []) = "({})"
printCallArgumentList (CallArgumentExprIdent identifierExpressionPairs) = "({" ++ (printIdentifierExpressionPairs identifierExpressionPairs) ++ "})"

printLiteral :: Literal -> String
printLiteral (StringLit stringLiteral) = (printStringLiteral stringLiteral)
printLiteral (NumberLit numberLiteral) = (printNumberLiteral numberLiteral)
printLiteral (BoolLit booleanLiteral) = (printBooleanLiteral booleanLiteral)
printLiteral (HexStringLit hexStringLiteral) = (printHexStringLiteral hexStringLiteral)
printLiteral (UnicodeStringLit unicodeStringLiteral) = (printUnicodeStringLiteral unicodeStringLiteral)

printBooleanLiteral :: BooleanLiteral -> String
printBooleanLiteral LanguageGrammar.True = "true"
printBooleanLiteral LanguageGrammar.False = "false"

printVariableDeclaration :: VariableDeclaration -> String
printVariableDeclaration (VariableDeclaration typeName (Just dataLocation) identifier) = (printTypeName typeName) ++ (printDataLocation dataLocation) ++ (printIdentifier identifier)
printVariableDeclaration (VariableDeclaration typeName (Nothing) identifier) = (printTypeName typeName) ++ (printIdentifier identifier)

printReceiveFunctionDefinition :: ReceiveFunctionDefinition -> String
printReceiveFunctionDefinition (ReceiveFunctionDefinition receiveModifiers (Just block)) = "receive ( )" ++ (printReceiveModifiersList receiveModifiers) ++ (printBlock block)
printReceiveFunctionDefinition (ReceiveFunctionDefinition receiveModifiers (Nothing)) = "receive ( )" ++ (printReceiveModifiersList receiveModifiers) ++ "; "

printReceiveModifier :: ReceiveModifiers -> String
printReceiveModifier (ExternalReceive) = "external"
printReceiveModifier (PayableReceive) = "payable"
printReceiveModifier (ModifierInvocReceive modifierInvocation) = (printModifierInvocation modifierInvocation)
printReceiveModifier (VirtualReceive) = "virtual"
printReceiveModifier (OverrideReceive overrideSpecifier) = (printOverrideSpecifier overrideSpecifier)

printReceiveModifiersList :: [ReceiveModifiers] -> String
printReceiveModifiersList [] = ""
printReceiveModifiersList [x] = (printReceiveModifier x)
printReceiveModifiersList (x:xs) = (printReceiveModifier x) ++ " " ++ (printReceiveModifiersList xs)

printStateVariableDeclaration :: StateVariableDeclaration -> String
printStateVariableDeclaration (StateVariableDeclaration typeName stateVariableModifiers identifier (Just expression)) = (printTypeName typeName) ++ (printStateVariableModifiersList stateVariableModifiers) ++ (printIdentifier identifier) ++ " = " ++ (printExpression expression) ++ "; "
printStateVariableDeclaration (StateVariableDeclaration typeName stateVariableModifiers identifier (Nothing)) = (printTypeName typeName) ++ (printStateVariableModifiersList stateVariableModifiers) ++ (printIdentifier identifier) ++ "; "

printStateVariableModifier :: StateVariableModifiers -> String
printStateVariableModifier (PublicState) = "public"
printStateVariableModifier (PrivateState) = "private"
printStateVariableModifier (InternalState) = "internal"
printStateVariableModifier (ConstantState) = "constant"
printStateVariableModifier (OverrideState overrideSpecifier) = (printOverrideSpecifier overrideSpecifier)
printStateVariableModifier (ImmutableState) = "immutable"

printStateVariableModifiersList :: [StateVariableModifiers] -> String
printStateVariableModifiersList [] = ""
printStateVariableModifiersList [x] = (printStateVariableModifier x)
printStateVariableModifiersList (x:xs) = (printStateVariableModifier x) ++ " " ++ (printStateVariableModifiersList xs)

printEventDefinition :: EventDefinition -> String
printEventDefinition (EventDefinition identifier eventParameters (Just anonymous)) = "event " ++ (printIdentifier identifier) ++ "(" ++ (printEventParameterList eventParameters) ++ ")" ++ " anonymous;"
printEventDefinition (EventDefinition identifier eventParameters (Nothing)) = "event " ++ (printIdentifier identifier) ++ "(" ++ (printEventParameterList eventParameters) ++ ")" ++ " anonymous;"

printEventParameter :: EventParameter -> String
printEventParameter (EventParameter typeName (Nothing) (Nothing)) = (printTypeName typeName)
printEventParameter (EventParameter typeName (Nothing) (Just identifier)) = (printTypeName typeName) ++ " " ++ (printIdentifier identifier)
printEventParameter (EventParameter typeName (Just indexed) (Nothing)) = (printTypeName typeName) ++ " indexed"
printEventParameter (EventParameter typeName (Just indexed) (Just identifier)) = (printTypeName typeName) ++ " indexed " ++ (printIdentifier identifier)

printEventParameterList :: [EventParameter] -> String
printEventParameterList [] = ""
printEventParameterList [x] = (printEventParameter x)
printEventParameterList (x:xs) = (printEventParameter x) ++ ", " ++ (printEventParameterList xs)

printUsingDirective :: UsingDirective -> String
printUsingDirective (UsingDirective identifierPath (Just typeName)) = "using " ++ (printIdentifierPath identifierPath) ++ " for " ++ (printTypeName typeName) ++ ";"
printUsingDirective (UsingDirective identifierPath (Nothing)) = "using " ++ (printIdentifierPath identifierPath) ++ " for *;"

printInheritanceSpecifier :: InheritanceSpecifier -> String
printInheritanceSpecifier (InheritanceSpecifier identifierPath (Just callArgumentList)) = (printIdentifierPath identifierPath) ++ " " ++ (printCallArgumentList callArgumentList)
printInheritanceSpecifier (InheritanceSpecifier identifierPath (Nothing)) = (printIdentifierPath identifierPath)

printTupleExpression :: TupleExpression -> String
printTupleExpression (TupleExpression []) = "[]"
printTupleExpression (TupleExpression [x]) = "[" ++ (printExpression x) ++"]"
printTupleExpression (TupleExpression expressions) = "[" ++ (printCommaExpressions expressions) ++ "]"

printInlineArrayExpression :: InlineArrayExpression -> String
printInlineArrayExpression (InlineArrayExpression expression []) = "[" ++ (printExpression expression) ++ "]"
printInlineArrayExpression (InlineArrayExpression expression expressions) = "[" ++ (printExpression expression) ++ ", " ++ (printCommaExpressions expressions) ++ "]"

printVariableDeclarationTuple :: VariableDeclarationTuple -> String
printVariableDeclarationTuple (VariableDeclarationTuple commas variableDeclaration commaVarDecs) = "(" ++ (printCommasList commas) ++ (printVariableDeclaration variableDeclaration) ++ (printCommaVarDeclarations commaVarDecs) ++ ")"

printCommasList :: [Comma] -> String
printCommasList [] = ""
printCommasList (x:xs) = "," ++ (printCommasList xs)

printCommaVarDeclarations :: [(Maybe Comma, Maybe VariableDeclaration)] -> String
printCommaVarDeclarations [] = ""
printCommaVarDeclarations ((Just comma, Just varDec):xs) = "," ++ (printVariableDeclaration varDec) ++ (printCommaVarDeclarations xs)
printCommaVarDeclarations ((Just comma, Nothing):xs) = "," ++ (printCommaVarDeclarations xs)
printCommaVarDeclarations ((Nothing, Just varDec):xs) = (printVariableDeclaration varDec) ++ (printCommaVarDeclarations xs)
printCommaVarDeclarations ((Nothing, Nothing):xs) = (printCommaVarDeclarations xs)

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

printStringLiteral :: StringLiteral -> String
printStringLiteral (StringLiteralDouble stringLitDouble) = (printStringLitDouble stringLitDouble)
printStringLiteral (StringLiteralSingle stringLitSingle) = (printStringLitSingle stringLitSingle)

printNumberLiteral :: NumberLiteral -> String
printNumberLiteral (Decimal decimalNumber (Just numberUnit)) = (printDecimalNumber decimalNumber) ++ (printNumberUnit numberUnit)
printNumberLiteral (Decimal decimalNumber (Nothing)) = (printDecimalNumber decimalNumber)
printNumberLiteral (HexaDecimal hexNumber (Just numberUnit)) = ((printHexNumber hexNumber) ++ (printNumberUnit numberUnit))
printNumberLiteral (HexaDecimal hexNumber (Nothing)) = (printHexNumber hexNumber)

printHexStringLiteral :: HexStringLiteral -> String
printHexStringLiteral (HexStringLiteral hexString []) = (printHexString hexString)
printHexStringLiteral (HexStringLiteral hexString hexStrings) = (printHexString hexString) ++ (printHexStringList hexStrings)

printHexStringList :: [HexString] -> String
printHexStringList [] = ""
printHexStringList (x:xs) = (printHexString x) ++ (printHexStringList xs)

printDecimalNumber :: DecimalNumber -> String -- possibly change
printDecimalNumber (DecimalNumber number) = number

printHexString :: HexString -> String
printHexString (DoubleQuotedHex charCharUnderscores) = "\'hex\' \"" ++ (printCharCharUnderscores charCharUnderscores) ++ "\""
printHexString (SingeQuoteHex charCharUnderscores) = "\'hex\' \\'" ++ (printCharCharUnderscores charCharUnderscores) ++ "\\'"

printCharCharUnderscore :: (Char, Char, Maybe Underscore) -> String
printCharCharUnderscore (char1, char2, Just underscore) = [char1] ++ [char2] ++ "_"
printCharCharUnderscore (char1, char2, Nothing) = [char1] ++ [char2]

printCharCharUnderscores :: [(Char, Char, Maybe Underscore)] -> String
printCharCharUnderscores [] = ""
printCharCharUnderscores (x:xs) = (printCharCharUnderscore x) ++ (printCharCharUnderscores xs)

printHexNumber :: HexNumber -> String
printHexNumber (HexNumber hexNum) = hexNum

printNumberUnit :: NumberUnit -> String
printNumberUnit Wei = "wei"
printNumberUnit GWei = "gwei"
printNumberUnit Ether = "ether"
printNumberUnit Seconds = "seconds"
printNumberUnit Minutes = "minutes"
printNumberUnit Hours = "hours"
printNumberUnit Days = "days"
printNumberUnit Weeks = "weeks"
printNumberUnit Years = "years"

printUnicodeStringLiteral :: UnicodeStringLiteral -> String
printUnicodeStringLiteral (UnicodeStringLiteral (Just char) escapeSequences) = "unicode\"" ++ (printEscapeSequenceList escapeSequences) ++ [char] ++ "\""
printUnicodeStringLiteral (UnicodeStringLiteral (Nothing) escapeSequences) = "unicode\"" ++ (printEscapeSequenceList escapeSequences) ++ "\""

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