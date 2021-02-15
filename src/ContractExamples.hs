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
shortenedStoredData = stateUintDeclaration "storedData" []

shortenedSetParam = createParameterList [(uint, "x")]
shortenedSetBlock = createBlock [expressionToStatement $ (createEqualsExpression (createIdentifier "storedData") (createIdentifier "x"))]
shortenedSetFunc = createVoidFunction "set" [public] (Just setParam) setBlock

shortenedGetParam = createParameterList [(uint, "")]
shortenedGetBlock = createBlock [(Return (ReturnStatement (Just (IdentifierExpr $ createIdentifier "storedData"))))]
shortenedGetFunc = createReturnFunction "get" [public, view] (Nothing) shortenedGetBlock (shortenedGetParam)
-- Coin Example --

















-- Voting Example --
ballotExample = createPragma "solidity >=0.7.0 <0.9.0" ballotContract
ballotContract = defineContract "Ballot" [StructElem voterStruct, StructElem proposalStruct, StateVariableElem chairpersonVariable, StateVariableElem votersMapping, StateVariableElem proposals, ConstructElem ballotConstructor, FunctionElem giveRightToVote, FunctionElem delegateFunction, FunctionElem voteFunction, FunctionElem functionWinningProposal, FunctionElem winnerNameFunction] EOF
voterStruct = createStruct "Voter" [(uint, "weight"), (bool, "voted"), (address, "delegate"), (uint, "vote")]
proposalStruct = createStruct "Proposal" [(bytes32, "name"), (uint, "voteCount")]
chairpersonVariable = stateAddressDeclaration "chairperson" [PublicState]
mappingTypeName = createMappingFromType address (IdentifierPathType $ IdentifierPath (createIdentifier "Voter") [])
votersMapping = stateMappingDeclaration "voters" mappingTypeName [PublicState]
proposals = stateStructArrayDeclaration "Proposal" [PublicState] "proposals"

proposalNamesParams = createParameterList [(bytes32, "proposalNames")]
chairpersonAssign = expressionToStatement (createEqualsExpression (createIdentifier "chairperson") (createIdentifier "msg.sender"))
weightAssign = expressionToStatement $ Equals (DotIdentifier (Index (IdentifierExpr (createIdentifier "voters")) (IdentifierExpr (createIdentifier "chairperson"))) (createIdentifier "weight")) (LiteralExpr $ createDecimalNumber "1")
forInitalise = VarInitialse $ uintVariableAssignmentDeclaration "i" (LiteralExpr $ createDecimalNumber "1")
forExpressionStatement = ExpressionStatement (LessThan (IdentifierExpr (createIdentifier "i")) (DotIdentifier (IdentifierExpr (createIdentifier "proposalNames")) (createIdentifier "length")))
forIncrement = PostIncrement (IdentifierExpr (createIdentifier "i"))
createProposal = ExpressionArgs (createIdentifierExpression "Proposal") (CallArgumentExprIdent [(createIdentifier "name", (Index (createIdentifierExpression "proposalNames") (createIdentifierExpression "i"))), (createIdentifier "voteCount", LiteralExpr $ createDecimalNumber "0")])
callPush = expressionToStatement $ ExpressionArgs (DotIdentifier (createIdentifierExpression "proposals") (createIdentifier "push")) (CallArgumentExpr (createProposal) [])
forLoop = For (ForStatement (forInitalise) (Just forExpressionStatement) (Just forIncrement) (BlockStatement $ createBlock [callPush]))
ballotConstructor = Constructor (Just proposalNamesParams) (Nothing) (createBlock [chairpersonAssign, weightAssign, forLoop])

giveRightToVoteParams = createParameterList [(address, "voter")]
require1 = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (createEqualityExpression (createIdentifier "msg.sender") (createIdentifier "chairperson")) [(LiteralExpr $ createDoubleQuotedStringLiteral "Only chairperson can give right to vote.")])
require2 = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (Not (DotIdentifier (Index (createIdentifierExpression "voters") (createIdentifierExpression "voter")) (createIdentifier "voted"))) [(LiteralExpr $ createDoubleQuotedStringLiteral "The voter already voted.")])
require3 = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (Equality (DotIdentifier (Index (createIdentifierExpression "voters") (createIdentifierExpression "voter")) (createIdentifier "weight")) (LiteralExpr $ createDecimalNumber "0")) [])
voterWeightAssign = expressionToStatement $ Equals (DotIdentifier (Index (IdentifierExpr (createIdentifier "voters")) (IdentifierExpr (createIdentifier "voter"))) (createIdentifier "weight")) (LiteralExpr $ createDecimalNumber "1")
giveRightToVote = createVoidFunction "giveRightToVote" [public] (Just giveRightToVoteParams) (createBlock [require1, require2, require3, voterWeightAssign])

senderAssign = VarDec $ identifierPathAssignmentDeclaration "Voter" "sender" (Index (IdentifierExpr (createIdentifier "voters")) (IdentifierExpr (createIdentifier "msg.sender")))
requireNotVoted = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (Not (DotIdentifier (createIdentifierExpression "sender") (createIdentifier "voted"))) [(LiteralExpr $ createDoubleQuotedStringLiteral "You already voted.")])
requireNoSelfDelegation = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (InEquality (createIdentifierExpression "to") (createIdentifierExpression "msg.sender")) [(LiteralExpr $ createDoubleQuotedStringLiteral "Self-delegation is disallowed.")])
toAssign = Equals (createIdentifierExpression "to") (DotIdentifier (Index (createIdentifierExpression "voters") (createIdentifierExpression "to")) (createIdentifier "delegate"))
requireLoop = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (InEquality (createIdentifierExpression "to") (createIdentifierExpression "msg.sender")) [(LiteralExpr $ createDoubleQuotedStringLiteral "Found loop in delegation.")])
-- InEquality (DotIdentifier (Index (createIdentifierExpression "voters") (createIdentifierExpression "to")) (createIdentifier "delegate"))) (ExpressionArgs (createIdentifierExpression "address") (expressionToStatement $ CallArgumentExpr (createDecimalNumber "0") []))
whileLoop = While(WhileStatement (InEquality (DotIdentifier (Index (createIdentifierExpression "voters") (createIdentifierExpression "to")) (createIdentifier "delegate")) (ExpressionArgs (createIdentifierExpression "address") (CallArgumentExpr (LiteralExpr (createDecimalNumber "0")) []))) (BlockStatement $ createBlock [expressionToStatement toAssign, requireLoop]))
voteTrue = Equals (DotIdentifier (createIdentifierExpression "sender") (createIdentifier "voted")) (LiteralExpr $ createBoolLiteral Prelude.True)
assignDelegate = Equals (DotIdentifier (createIdentifierExpression "sender") (createIdentifier "delegate")) (createIdentifierExpression "to")
declareDelegate = VarDec $ identifierPathAssignmentDeclaration "Voter" "delegate_" (Index (IdentifierExpr (createIdentifier "voters")) (IdentifierExpr (createIdentifier "to")))
proposalExpression = DotIdentifier (Index (createIdentifierExpression "proposals") (createIdentifierExpression "delegate_.vote")) (createIdentifier "voteCount")
elseDidVote = expressionToStatement (PlusEquals (DotIdentifier (Index (createIdentifierExpression "proposals") (createIdentifierExpression "delegate_.vote")) (createIdentifier "voteCount")) (DotIdentifier (createIdentifierExpression "sender") (createIdentifier "weight")))
ifVoted = If (IfStatement (DotIdentifier (createIdentifierExpression "delegate_") (createIdentifier "voted")) (expressionToStatement $ PlusEquals (proposalExpression) (DotIdentifier (createIdentifierExpression "sender") (createIdentifier "weight"))) (Just elseDidVote))
delegateFunction = createVoidFunction "delegate" [public] (Just $createParameterList [(address, "to")]) (createBlock [senderAssign, requireNotVoted, requireNoSelfDelegation, whileLoop, expressionToStatement voteTrue, expressionToStatement assignDelegate, declareDelegate, ifVoted])

assignSender = VarDec $ identifierPathAssignmentDeclaration "Voter" "sender" (Index (IdentifierExpr (createIdentifier "voters")) (IdentifierExpr (createIdentifier "msg.sender")))
requireWeightNotZero = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (InEquality (DotIdentifier (createIdentifierExpression "sender") (createIdentifier "weight")) (LiteralExpr $ createDecimalNumber "0")) [(LiteralExpr $ createDoubleQuotedStringLiteral "Has no right to vote")])
requireSenderNotVoted = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (Not (DotIdentifier (createIdentifierExpression "sender") (createIdentifier "voted"))) [(LiteralExpr $ createDoubleQuotedStringLiteral "Already voted.")])
markVoted = expressionToStatement $ Equals (DotIdentifier (createIdentifierExpression "sender") (createIdentifier "voted")) (LiteralExpr $ createBoolLiteral Prelude.True)
voteProposal = expressionToStatement $ Equals (DotIdentifier (createIdentifierExpression "sender") (createIdentifier "vote")) (createIdentifierExpression "proposal")
addWeight = expressionToStatement $ PlusEquals (DotIdentifier (Index (createIdentifierExpression "proposals") (createIdentifierExpression "proposal")) (createIdentifier "voteCount")) (DotIdentifier (createIdentifierExpression "sender") (createIdentifier "weight"))
voteFunction = createReturnFunction "vote" [public] (Nothing) (createBlock [assignSender, requireWeightNotZero, requireSenderNotVoted, markVoted, voteProposal, addWeight]) (createParameterList [(uint, "proposal")])

winningVoteCount = VarDec $ uintVariableAssignmentDeclaration "winningVoteCount" (LiteralExpr $ createDecimalNumber "0")
winningProposalIfVoteCountAssignment = expressionToStatement (Equals (createIdentifierExpression "winningVoteCount") (DotIdentifier (Index (createIdentifierExpression "proposals") (createIdentifierExpression "p")) (createIdentifier "voteCount")))
winningProposalIfProposalAssignment = expressionToStatement (Equals (createIdentifierExpression "winningProposal_") (createIdentifierExpression "p"))
winningProposalIf = If (IfStatement (GreaterThan (DotIdentifier (Index (createIdentifierExpression "proposals") (createIdentifierExpression "p")) (createIdentifier "voteCount")) (createIdentifierExpression "winningVoteCount")) (BlockStatement $ createBlock [winningProposalIfVoteCountAssignment, winningProposalIfProposalAssignment]) (Nothing))
forInitaliseP = VarInitialse $ uintVariableAssignmentDeclaration "p" (LiteralExpr $ createDecimalNumber "0")
forExpressionStatementP = ExpressionStatement (LessThan (IdentifierExpr (createIdentifier "p")) (DotIdentifier (IdentifierExpr (createIdentifier "proposals")) (createIdentifier "length")))
forIncrementP = PostIncrement (IdentifierExpr (createIdentifier "p"))
forProposals = For (ForStatement (forInitaliseP) (Just forExpressionStatementP) (Just forIncrementP) (BlockStatement $ createBlock [winningProposalIf]))
functionWinningProposal = createReturnFunction "winningProposal" [public, view] (Nothing) (createBlock [winningVoteCount, forProposals]) (createParameterList [(uint, "winningProposal_")])

winnerNameAssign = expressionToStatement (Equals (createIdentifierExpression "winnerName_") (DotIdentifier (Index (createIdentifierExpression "proposals") (createIdentifierExpression "winningProposal()")) (createIdentifier "name")))
winnerNameFunction = createReturnFunction "winnerName" [public, view] (Nothing) (createBlock [winnerNameAssign]) (createParameterList [(bytes32, "winnerName_")])
-- Voting Example --
