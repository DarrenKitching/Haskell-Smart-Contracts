module ContractExamples where

import LanguageGrammarPrinting
import LanguageGrammar
import Abstractions

-- Coin Example --

coinExample = (Pragma (PragmaToken 's') [(PragmaToken 'o'), (PragmaToken 'l'), (PragmaToken 'i'), (PragmaToken 'd'), (PragmaToken 'i'), (PragmaToken 't'), (PragmaToken 'y'), (PragmaToken ' '), (PragmaToken '^'), (PragmaToken '0'), (PragmaToken '.'), (PragmaToken '7'), (PragmaToken '.'), (PragmaToken '4')] contractDef)
contractDef = (ContractDef (ContractDefinition (Nothing) (Identifier 'S' ['i', 'm', 'p', 'l', 'e', 'S', 't', 'o','r', 'a', 'g', 'e']) (Nothing) [storedData, setFunc, getFunc]) EOF)
storedData = (StateVariableElem (StateVariableDeclaration (ElementaryType $ UnsignedIntType UInt) [] (Identifier 's' ['t', 'o', 'r', 'e', 'd', 'D', 'a', 't','a']) (Nothing)))

setParam = ParameterList (ElementaryType $ UnsignedIntType UInt) (Nothing) (Just (Identifier 'x' [])) (Nothing)
setBlock = Block [BlockStatementItem (ExprStatement (ExpressionStatement (Equals (IdentifierExpr (Identifier 's' ['t', 'o', 'r', 'e', 'd', 'D', 'a', 't','a'])) (IdentifierExpr (Identifier 'x' [])))))]
setFunc = (FunctionElem (FunctionaDefinition (IdentifierName (Identifier 's' ['e', 't'])) (Just setParam) [(VisibilityModifier PublicVisibility)] (Nothing) (Just setBlock)))

getParam = ParameterList (ElementaryType $ UnsignedIntType UInt) (Nothing) (Nothing) (Nothing)
getBlock = Block [(BlockStatementItem (Return (ReturnStatement (Just (IdentifierExpr (Identifier 's' ['t', 'o', 'r', 'e', 'd', 'D', 'a', 't','a']))))))]
getFunc = (FunctionElem (FunctionaDefinition (IdentifierName (Identifier 'g' ['e', 't'])) (Nothing) [(VisibilityModifier PublicVisibility), (StateMutabilityModifier View)] (Just (getParam)) (Just getBlock)))

shorternedCoinExample = createPragma "solidity ^0.7.4" shortenedContractDef
shortenedContractDef = defineContract "SimpleStorage" [StateVariableElem shortenedStoredData, FunctionElem shortenedSetFunc, FunctionElem shortenedGetFunc] EOF
shortenedStoredData = uintDeclaration "storedData" []

shortenedSetParam = createParameterList [(uint, "x")]
shortenedSetBlock = createBlock [ExprStatement (createEqualsExpression (createIdentifier "storedData") (createIdentifier "x"))]
shortenedSetFunc = createVoidFunction "set" [public] (Just setParam) setBlock

shortenedGetParam = createParameterList [(uint, "")]
shortenedGetBlock = createBlock [(Return (ReturnStatement (Just (IdentifierExpr $ createIdentifier "storedData"))))]
shortenedGetFunc = createReturnFunction "get" [public, view] (Nothing) shortenedGetBlock (shortenedGetParam)
-- Coin Example --

















-- Voting Example --
ballotExample = createPragma "solidity >=0.7.0 <0.9.0" ballotContract
ballotContract = defineContract "Ballot" [StructElem voterStruct, StructElem proposalStruct, StateVariableElem votersMapping, StateVariableElem proposals] EOF
voterStruct = createStruct "Voter" [(uint, "weight"), (bool, "voted"), (address, "delegate"), (uint, "vote")]
proposalStruct = createStruct "Proposal" [(bytes32, "name"), (uint, "voteCount")]
mappingTypeName = createMappingFromType address (IdentifierPathType $ IdentifierPath (createIdentifier "Voter") [])
votersMapping = mappingDeclaration "voters" mappingTypeName [PublicState]

proposals = structArrayDeclaration "Proposal" [PublicState] "proposals"
proposalNamesParams = createParameterList [(bytes32, "proposalNames")]
chairpersonAssign = ExprStatement (createEqualsExpression (createIdentifier "chairperson") (createIdentifier "msg.sender"))
weightAssign = ExprStatement $ ExpressionStatement $ Equals (DotIdentifier (Index (IdentifierExpr (createIdentifier "voters")) (IdentifierExpr (createIdentifier "chairperson"))) (createIdentifier "weight")) (LiteralExpr $ NumberLit (Decimal (DecimalNumber ['1']) Nothing))
forInitalise = (VarInitialse $ SingleVariableDeclartion (VariableDeclaration (ElementaryType uint) (Nothing) (createIdentifier "i")) (LiteralExpr $ NumberLit (Decimal (DecimalNumber ['1']) Nothing)))
forExpressionStatement = ExprStatement $ ExpressionStatement (LessThan (IdentifierExpr (createIdentifier "i")) (DotIdentifier (IdentifierExpr (createIdentifier "proposalNames")) (createIdentifier "length")))
forIncrement = PostIncrement (IdentifierExpr (createIdentifier "i"))

-- proposalFuncCall = ExpressionArgs (DotIdentifier (IdentifierExpr (createIdentifier "proposalNames")) (createIdentifier "push")) (CallArgumentExpr (IdentifierExpr (createIdentifier "Proposal")) [Expression])
-- forLoop = For (ForStatement (forInitalise) (Maybe ExpressionStatement) (Maybe Expression) Statement)
--Constructor (Maybe ParameterList) (Maybe [Modifiers]) Block
-- Voting Example --
