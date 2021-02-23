module FinancialContracts where
import LanguageGrammar
import YulLanguageGrammar
import Abstractions

data Person = Owner | Anyone | Specified -- only the owner can perform this action, anyone can perfrom the action or a variable Specified user can perfrom the action
data Amount = All | SpecificAmount -- perform this action on the entire amount, the user's specified share in a variable or a specific amount supplied
data Source = ContractBalance | UserBalance
data When = Anytime | Before | After | Countdown -- action can be performed at anytime, before a specific time, after a specific time within a certain period
data Functionality = Withdraw | Deposit | Transfer | Blank -- Withdraw, deposit or transfer money
data Conditioned = HasWon | HasLost -- Only a winner can perform the aciton or only the loser can perform the action.
data ExchangeType = FromContract | FromUser -- specifies if an action e.g. withdrawal is from the contract balance or the user's personal balance

contractAmount = uintVariableAssignmentDeclaration "amount" (DotIdentifier (ExpressionArgs (createIdentifierExpression "address") (CallArgumentExpr (createIdentifierExpression "this") [])) (createIdentifier "balance"))
userAmount = uintVariableAssignmentDeclaration "userBalance" (Index (createIdentifierExpression "balance") (createIdentifierExpression "msg.sender"))
decrementUserAmount = expressionToStatement $ (MinusEquals (Index (createIdentifierExpression "balance") (createIdentifierExpression "msg.sender")) (createIdentifierExpression "userBalance"))
transferUserAmount = expressionToStatement $ DotIdentifier (PayableExpression (CallArgumentExpr (createIdentifierExpression "msg.sender") [])) (createIdentifier "transfer(userBalance)")
transferWithdrawal = expressionToStatement $ DotIdentifier (PayableExpression (CallArgumentExpr (createIdentifierExpression "msg.sender") [])) (createIdentifier "transfer(amount)")
transferContractBalance = expressionToStatement $ DotIdentifier (PayableExpression (CallArgumentExpr (createIdentifierExpression "msg.sender") [])) (createIdentifier "transfer(contractBalance)")
requireSufficientBalance = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (GreaterThanEQualTo (Index (createIdentifierExpression "balance") (createIdentifierExpression "msg.sender")) (createIdentifierExpression "amount")) [])
decrementBalance = expressionToStatement $ (MinusEquals (Index (createIdentifierExpression "balance") (createIdentifierExpression "msg.sender")) (createIdentifierExpression "amount"))
requireSufficientContractBalance = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (GreaterThanEQualTo (createIdentifierExpression "contractBalance") (createIdentifierExpression "amount")) [])
decrementContractBalance = expressionToStatement $ (MinusEquals (createIdentifierExpression "contractBalance") (createIdentifierExpression "amount"))
sendParams = createParameterList [(addressPayable, "_to")]
sendFunds = expressionToStatement $ Equals (TupleExpr (TupleExpression [createIdentifierExpression "bool sent", createIdentifierExpression " "])) (ExpressionArgs (IdentifierExpression (createIdentifierExpression "_to.call") [(createIdentifier "value", createIdentifierExpression "msg.value")]) (CallArgumentExpr (createIdentifierExpression "\"\"") []))
requireSent = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (createIdentifierExpression "sent") [(createIdentifierExpression "\"Failed to send Ether\"")])
depositUserShare = expressionToStatement $ (PlusEquals (Index (createIdentifierExpression "balance") (createIdentifierExpression "msg.sender")) (createIdentifierExpression "msg.value"))
incrementContractBalance = expressionToStatement $ (PlusEquals (createIdentifierExpression "contractBalance") (createIdentifierExpression "msg.value"))

createFunction :: String -> Functionality -> Amount -> Source -> ContractBodyElement
createFunction name Withdraw All UserBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Nothing) [VisibilityModifier PublicVisibility] (Nothing) (Just $ createBlock [VarDec $ userAmount, decrementUserAmount, transferUserAmount])
createFunction name Withdraw SpecificAmount UserBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Just $ createParameterList [(uint, "amount")]) [VisibilityModifier PublicVisibility] (Nothing) (Just $ createBlock [requireSufficientBalance, decrementBalance, transferWithdrawal])
createFunction name Withdraw All ContractBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Nothing) [VisibilityModifier PublicVisibility] (Nothing) (Just $ createBlock [transferContractBalance, contractBalanceAssignment])
createFunction name Withdraw SpecificAmount ContractBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Just $ createParameterList [(uint, "amount")]) [VisibilityModifier PublicVisibility] (Nothing) (Just $ createBlock [requireSufficientContractBalance, transferWithdrawal, decrementContractBalance])

createFunction name Deposit All UserBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Nothing) [VisibilityModifier PublicVisibility, StateMutabilityModifier PayableMutability] (Nothing) (Just $ createBlock [depositUserShare])
createFunction name Deposit SpecificAmount UserBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Just $ createParameterList [(uint, "amount")]) [VisibilityModifier PublicVisibility, StateMutabilityModifier PayableMutability] (Nothing) (Just $ createBlock [(expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (Equality (createIdentifierExpression "msg.value") (createIdentifierExpression "amount")) [])), depositUserShare])
createFunction name Deposit All ContractBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Nothing) [VisibilityModifier PublicVisibility, StateMutabilityModifier PayableMutability] (Nothing) (Just $ createBlock [incrementContractBalance])
createFunction name Deposit SpecificAmount ContractBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Just $ createParameterList [(uint, "amount")]) [VisibilityModifier PublicVisibility, StateMutabilityModifier PayableMutability] (Nothing) (Just $ createBlock [(expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (Equality (createIdentifierExpression "msg.value") (createIdentifierExpression "amount")) [])), incrementContractBalance])

createFunction name Transfer All ContractBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Just sendParams) [VisibilityModifier PublicVisibility, StateMutabilityModifier PayableMutability] (Nothing) (Just $ createBlock [(expressionToStatement $ (PlusEquals (Index (createIdentifierExpression "balance") (createIdentifierExpression "_to")) (createIdentifierExpression "contractBalance"))), contractBalanceAssignment])
createFunction name Transfer SpecificAmount ContractBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Just $ createParameterList [(addressPayable, "_to"), (uint, "amount")]) [VisibilityModifier PublicVisibility, StateMutabilityModifier PayableMutability] (Nothing) (Just $ createBlock [requireSufficientContractBalance, decrementContractBalance, expressionToStatement $ (PlusEquals (Index (createIdentifierExpression "balance") (createIdentifierExpression "_to")) (createIdentifierExpression "amount"))])

createFunction name Transfer All UserBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Just sendParams) [VisibilityModifier PublicVisibility, StateMutabilityModifier PayableMutability] (Nothing) (Just $ createBlock [VarDec $ userAmount, decrementUserAmount, (expressionToStatement $ (PlusEquals (Index (createIdentifierExpression "balance") (createIdentifierExpression "_to")) (createIdentifierExpression "userBalance")))])
createFunction name Transfer SpecificAmount UserBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Just $ createParameterList [(addressPayable, "_to"), (uint, "amount")]) [VisibilityModifier PublicVisibility, StateMutabilityModifier PayableMutability] (Nothing) (Just $ createBlock [requireSufficientBalance, decrementBalance, (expressionToStatement $ PlusEquals (Index (createIdentifierExpression "balance") (createIdentifierExpression "_to")) (createIdentifierExpression "amount"))])

createFunction name Blank _ _ = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Nothing) [VisibilityModifier PublicVisibility] (Nothing) (Nothing)

createContract :: String -> [ContractBodyElement] -> Contract
createContract name functions = createPragma "solidity ^0.7.4" $ ContractDef (ContractDefinition (Nothing) (createIdentifier name) (Nothing) ((buildConstructor functions):functions)) EOF

combine :: Contract -> Contract -> Contract
combine (FunctionDef function EOF) (contract2) = (FunctionDef function contract2)

ifBeforeEnd :: ContractBodyElement -> ContractBodyElement
ifBeforeEnd x = addRequirement x (expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (LessthanEqualTo (createIdentifierExpression "block.timestamp") (createIdentifierExpression "end")) []))

ifAfterStart :: ContractBodyElement -> ContractBodyElement
ifAfterStart x = addRequirement x (expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (GreaterThanEQualTo (createIdentifierExpression "block.timestamp") (createIdentifierExpression "start")) []))

ifBetweenDates :: ContractBodyElement -> ContractBodyElement
ifBetweenDates x = addRequirement x (expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (AND (GreaterThanEQualTo (createIdentifierExpression "block.timestamp") (createIdentifierExpression "start")) (LessthanEqualTo (createIdentifierExpression "block.timestamp") (createIdentifierExpression "end"))) []))

ifOwner :: ContractBodyElement -> ContractBodyElement
ifOwner x = addRequirement x (expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (Equality (createIdentifierExpression "owner") (createIdentifierExpression "msg.sender")) []))

addRequirement :: ContractBodyElement -> Statement -> ContractBodyElement
addRequirement (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Just block))) statement = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (addStatementToBlock block statement)))
addRequirement (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Nothing))) statement = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [statement])))

addStatementToBlock :: Block -> Statement -> Block
addStatementToBlock (Block xs) x = Block ((BlockStatementItem x):xs)

setStartTime :: ContractBodyElement
setStartTime = StateVariableElem $ StateVariableDeclaration (ElementaryType uint) [PublicState] (createIdentifier "start") (Nothing)

setEndTime :: ContractBodyElement
setEndTime = StateVariableElem $ StateVariableDeclaration (ElementaryType uint) [PublicState] (createIdentifier "end") (Nothing)

setContractBalance :: ContractBodyElement
setContractBalance = StateVariableElem $ StateVariableDeclaration (ElementaryType uint) [PublicState] (createIdentifier "contractBalance") (Nothing)

setOwner :: ContractBodyElement
setOwner = StateVariableElem $ StateVariableDeclaration (ElementaryType addressPayable) [PublicState] (createIdentifier "owner") (Nothing)

setReceipient :: String -> ContractBodyElement
setReceipient name = StateVariableElem $ StateVariableDeclaration (ElementaryType addressPayable) [PublicState] (createIdentifier name) (Nothing)

setBalance :: ContractBodyElement
setBalance = StateVariableElem $ stateMappingDeclaration "balance" (createMappingFromType address (ElementaryType uint)) []

setAddresses :: String -> ContractBodyElement
setAddresses name = StateVariableElem $ stateMappingDeclaration name (createMappingFromType address (ElementaryType uint)) []

loop :: Int -> ContractBodyElement -> ContractBodyElement
loop x (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Just block))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (addBlockToLoop (createLoop x) (block))))
loop x (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Nothing))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (addBlockToLoop (createLoop x) (createBlock []))))

startAssignment = expressionToStatement $ Equals (createIdentifierExpression "start") (createIdentifierExpression "_start")
endAssignment = expressionToStatement $ Equals (createIdentifierExpression "end") (createIdentifierExpression "_end")
contractBalanceAssignment = expressionToStatement $ Equals (createIdentifierExpression "contractBalance") (createIdentifierExpression "0")
ownerAssignment = expressionToStatement $ Equals (createIdentifierExpression "owner") (createIdentifierExpression "msg.sender")

buildConstructor :: [ContractBodyElement] -> ContractBodyElement
buildConstructor ((StateVariableElem (StateVariableDeclaration (ElementaryType uint) [PublicState] (Identifier 's' ['t', 'a', 'r', 't']) (Nothing))):xs) = joinConstructors (ConstructElem (Constructor (Just $ createParameterList [(uint, "_start")]) (Nothing) (createBlock [startAssignment]))) (buildConstructor xs)
buildConstructor ((StateVariableElem (StateVariableDeclaration (ElementaryType uint) [PublicState] (Identifier 'e' ['n', 'd']) (Nothing))):xs) = joinConstructors (ConstructElem (Constructor (Just $ createParameterList [(uint, "_end")]) (Nothing) (createBlock [endAssignment]))) (buildConstructor xs)
buildConstructor ((StateVariableElem (StateVariableDeclaration (ElementaryType uint) [PublicState] (Identifier 'c' ['o', 'n', 't', 'r', 'a', 'c', 't', 'B', 'a', 'l', 'a', 'n', 'c', 'e']) (Nothing))):xs) = joinConstructors (ConstructElem (Constructor (Nothing) (Nothing) (createBlock [contractBalanceAssignment]))) (buildConstructor xs)
buildConstructor ((StateVariableElem (StateVariableDeclaration (ElementaryType addressPayable) [PublicState] (Identifier 'o' ['w', 'n', 'e', 'r']) (Nothing))):xs) = joinConstructors (ConstructElem (Constructor (Nothing) (Nothing) (createBlock [ownerAssignment]))) (buildConstructor xs)
buildConstructor ((StateVariableElem (StateVariableDeclaration (ElementaryType addressPayable) [PublicState] (Identifier y ys) (Nothing))):xs) = joinConstructors (ConstructElem (Constructor (Just $ createParameterList [(addressPayable, (['_'] ++ y:ys))]) (Nothing) (createBlock [(expressionToStatement $ Equals (createIdentifierExpression (y:ys)) (createIdentifierExpression (['_'] ++[y] ++ ys)))]))) (buildConstructor xs)
buildConstructor (_:xs) = buildConstructor xs
buildConstructor [] = ConstructElem (Constructor (Nothing) (Nothing) (createBlock []))

createLoop :: Int -> Statement
createLoop x = For (ForStatement (VarInitialse (intVariableAssignmentDeclaration "i" (createIdentifierExpression "0"))) (Just $ ExpressionStatement (LessThan (createIdentifierExpression "i") (createIdentifierExpression (show x)))) (Just (PostIncrement (createIdentifierExpression "i"))) (BlockStatement $ createBlock []))

addBlockToLoop :: Statement -> Block -> Block
addBlockToLoop (For (ForStatement initialise (exprStatement) (expr) oldBlock)) block = createBlock [For $ (ForStatement initialise (exprStatement) (expr) (BlockStatement block))]

requireNotOwner = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (InEquality (createIdentifierExpression "owner") (createIdentifierExpression "msg.sender")) [])

giveOwnership :: ContractBodyElement
giveOwnership = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier "giveOwnership")) (Just $ createParameterList [(addressPayable, "newOwner")]) [VisibilityModifier PublicVisibility] (Nothing) (Just $ createBlock [requireNotOwner, (expressionToStatement $ Equals (createIdentifierExpression "owner") (createIdentifierExpression "newOwner"))])

proportionalTo :: String -> ContractBodyElement -> ContractBodyElement
proportionalTo var x = addRequirement x (expressionToStatement $ DivEquals (createIdentifierExpression "amount") (Div (Index (createIdentifierExpression var) (createIdentifierExpression "_to")) (createIdentifierExpression "100")))
