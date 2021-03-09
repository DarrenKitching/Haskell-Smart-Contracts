module FinancialContracts where
import LanguageGrammar
import LanguageGrammarPrinting
import YulLanguageGrammar
import Abstractions

data Language = CreateContract String [Language]
              | CreateFinancialFunction String FinancialFunctionality Amount Source
              | CreateCoreFunction String CoreFunctionality
              | Set Variable
              | ProportionalTo Rate Language
              | Loop Int Language
              | Conditioned Condition Language
              | Join String Language Language

data Recipient = Recipient String
data Condition = RequireOwner | RequireStarted | RequireNotStarted | RequireEnded | RequireNotEnded | RequireBetweenDates | RequireRecipient Recipient
               | RequireTrue Variable | RequireFalse Variable | ValueGreaterThan Variable Variable | ValueLessThan Variable Variable
               | ValueGreaterThanEqualTo Variable Variable | ValueLessThanEqualTo Variable Variable | EqualTo Variable Variable | NotEqualTo Variable Variable
data Variable = Owner | ContractBal | StartTime | EndTime | RecipientAddress Recipient | Balances | Addresses String | SignedAmount String
              | UnSignedAmount String | BooleanVariable String | Interest | Tax | DIRT | SharesOwned | TotalShares | MessageValue | BoolTrue | BoolFalse
data CoreFunctionality = GiveOwnership | GetBalance | GetVariable Variable | BecomeRecipient Recipient | Update Variable Variable
data Amount = All | SpecificAmount
data Source = ContractBalance | UserBalance
data FinancialFunctionality = Withdraw | Deposit | Transfer | BlankPayable
data Rate = InterestRate | TaxRate | Shares | InterestRateLessDIRT

outputContract :: Language -> String -> IO ()
outputContract (CreateContract name elements) destination = do
  writeFile destination ""
  appendFile destination "// SPDX-License-Identifier: GPL-3.0\n"
  appendFile destination (printSolidity (createContract name (convertToSolidity elements)))

convertToSolidity :: [Language] -> [ContractBodyElement]
convertToSolidity [] = []
convertToSolidity ((Set (MessageValue)):xs) = (convertToSolidity xs) -- special case, MessageValue cannot be set like other variables
convertToSolidity (x:xs) = (convertElementToSolidity x) : (convertToSolidity xs)

convertElementToSolidity :: Language -> ContractBodyElement
convertElementToSolidity (CreateFinancialFunction name functionality amount source) = createFunction name functionality amount source
convertElementToSolidity (CreateCoreFunction name GiveOwnership) = giveOwnership name
convertElementToSolidity (CreateCoreFunction name GetBalance) = (getBalance name)
convertElementToSolidity (CreateCoreFunction name (BecomeRecipient (recipient))) = (becomeRecipient name recipient)
convertElementToSolidity (CreateCoreFunction name (GetVariable (variable))) = (getVariable name variable)
convertElementToSolidity (CreateCoreFunction name (Update (var1) (var2))) = (updateElement name var1 var2)
convertElementToSolidity (Set Owner) = setOwner
convertElementToSolidity (Set ContractBal) = setContractBalance
convertElementToSolidity (Set StartTime) = setStartTime
convertElementToSolidity (Set EndTime) = setEndTime
convertElementToSolidity (Set (RecipientAddress (Recipient name))) = (setRecipient name)
convertElementToSolidity (Set Balances) = setBalance
convertElementToSolidity (Set (Addresses name)) = setAddresses name
convertElementToSolidity (Set Interest) = setInterestRate
convertElementToSolidity (Set Tax) = setTaxRate
convertElementToSolidity (Set DIRT) = setDIRT
convertElementToSolidity (Set SharesOwned) = setShares
convertElementToSolidity (Set TotalShares) = setTotalShares
convertElementToSolidity (Set (UnSignedAmount name)) = setUnSignedAmount name
convertElementToSolidity (Set (SignedAmount name)) = setSignedAmount name
convertElementToSolidity (Set (BooleanVariable name)) = setBooleanVariable name
convertElementToSolidity (ProportionalTo rate language) = proportionalTo rate (convertElementToSolidity language)
convertElementToSolidity (Loop times language) = loop times (convertElementToSolidity language)
convertElementToSolidity (Conditioned (RequireOwner) language) = ifOwner (convertElementToSolidity language)
convertElementToSolidity (Conditioned (RequireStarted) language) = ifAfterStart (convertElementToSolidity language)
convertElementToSolidity (Conditioned (RequireNotStarted) language) = ifBeforeStart (convertElementToSolidity language)
convertElementToSolidity (Conditioned (RequireEnded) language) = ifAfterEnd (convertElementToSolidity language)
convertElementToSolidity (Conditioned (RequireNotEnded) language) = ifBeforeEnd (convertElementToSolidity language)
convertElementToSolidity (Conditioned (RequireBetweenDates) language) = ifBetweenDates (convertElementToSolidity language)
convertElementToSolidity (Conditioned (RequireRecipient recipient) language) = ifRecipient (recipient) (convertElementToSolidity language)
convertElementToSolidity (Conditioned (RequireTrue var) language) = ifTrue (var) (convertElementToSolidity language)
convertElementToSolidity (Conditioned (RequireFalse var) language) = ifFalse (var) (convertElementToSolidity language)
convertElementToSolidity (Conditioned (ValueGreaterThan var1 var2) language) = ifValueGreaterThan (var1) (var2)  (convertElementToSolidity language)
convertElementToSolidity (Conditioned (ValueLessThan var1 var2) language) = ifValueLessThan (var1) (var2)  (convertElementToSolidity language)
convertElementToSolidity (Conditioned (ValueGreaterThanEqualTo var1 var2) language) = ifValueGreaterThanEqualTo (var1) (var2)  (convertElementToSolidity language)
convertElementToSolidity (Conditioned (ValueLessThanEqualTo var1 var2) language) = ifValueLessThanEqualTo (var1) (var2)  (convertElementToSolidity language)
convertElementToSolidity (Conditioned (EqualTo var1 var2) language) = ifEqualTo (var1) (var2)  (convertElementToSolidity language)
convertElementToSolidity (Conditioned (NotEqualTo var1 var2) language) = ifNotEqualTo (var1) (var2)  (convertElementToSolidity language)
convertElementToSolidity (Join name (function1) (function2)) = join (name) (convertElementToSolidity function1) (convertElementToSolidity function2)

contractAmount = uintVariableAssignmentDeclaration "amount" (DotIdentifier (ExpressionArgs (createIdentifierExpression "address") (CallArgumentExpr (createIdentifierExpression "this") [])) (createIdentifier "balance"))
userAmount = uintVariableAssignmentDeclaration "amount" (Index (createIdentifierExpression "balance") (createIdentifierExpression "msg.sender"))
decrementUserAmount = expressionToStatement $ (MinusEquals (Index (createIdentifierExpression "balance") (createIdentifierExpression "msg.sender")) (createIdentifierExpression "amount"))
transferUserAmount = expressionToStatement $ DotIdentifier (PayableExpression (CallArgumentExpr (createIdentifierExpression "msg.sender") [])) (createIdentifier "transfer(amount)")
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

createFunction :: String -> FinancialFunctionality -> Amount -> Source -> ContractBodyElement
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
createFunction name Transfer All UserBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Just sendParams) [VisibilityModifier PublicVisibility, StateMutabilityModifier PayableMutability] (Nothing) (Just $ createBlock [VarDec $ userAmount, decrementUserAmount, (expressionToStatement $ (PlusEquals (Index (createIdentifierExpression "balance") (createIdentifierExpression "_to")) (createIdentifierExpression "amount")))])
createFunction name Transfer SpecificAmount UserBalance = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Just $ createParameterList [(addressPayable, "_to"), (uint, "amount")]) [VisibilityModifier PublicVisibility, StateMutabilityModifier PayableMutability] (Nothing) (Just $ createBlock [requireSufficientBalance, decrementBalance, (expressionToStatement $ PlusEquals (Index (createIdentifierExpression "balance") (createIdentifierExpression "_to")) (createIdentifierExpression "amount"))])
createFunction name BlankPayable _ _ = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Nothing) [VisibilityModifier PublicVisibility, StateMutabilityModifier PayableMutability] (Nothing) (Nothing)

createContract :: String -> [ContractBodyElement] -> Contract
createContract name functions = createPragma "solidity ^0.7.4" $ ContractDef (ContractDefinition (Nothing) (createIdentifier name) (Nothing) ((buildConstructor functions):functions)) EOF

combine :: Contract -> Contract -> Contract
combine (FunctionDef function EOF) (contract2) = (FunctionDef function contract2)

ifAfterEnd :: ContractBodyElement -> ContractBodyElement
ifAfterEnd x = addRequirement x (expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (GreaterThan (createIdentifierExpression "block.timestamp") (createIdentifierExpression "end")) []))

ifBeforeEnd :: ContractBodyElement -> ContractBodyElement
ifBeforeEnd x = addRequirement x (expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (LessthanEqualTo (createIdentifierExpression "block.timestamp") (createIdentifierExpression "end")) []))

ifAfterStart :: ContractBodyElement -> ContractBodyElement
ifAfterStart x = addRequirement x (expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (GreaterThanEQualTo (createIdentifierExpression "block.timestamp") (createIdentifierExpression "start")) []))

ifBeforeStart :: ContractBodyElement -> ContractBodyElement
ifBeforeStart x = addRequirement x (expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (LessThan (createIdentifierExpression "block.timestamp") (createIdentifierExpression "start")) []))

ifBetweenDates :: ContractBodyElement -> ContractBodyElement
ifBetweenDates x = addRequirement x (expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (AND (GreaterThanEQualTo (createIdentifierExpression "block.timestamp") (createIdentifierExpression "start")) (LessthanEqualTo (createIdentifierExpression "block.timestamp") (createIdentifierExpression "end"))) []))

ifOwner :: ContractBodyElement -> ContractBodyElement
ifOwner x = addRequirement x requireOwner

ifRecipient :: Recipient -> ContractBodyElement -> ContractBodyElement
ifRecipient recipient x = addRequirement x (requireRecipient recipient)

ifTrue :: Variable -> ContractBodyElement -> ContractBodyElement
ifTrue var x = addRequirement x (expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (Equality (createIdentifierExpression (getVariableName var)) (createIdentifierExpression "true")) []))

ifFalse :: Variable -> ContractBodyElement -> ContractBodyElement
ifFalse var x = addRequirement x (expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (Equality (createIdentifierExpression (getVariableName var)) (createIdentifierExpression "false")) []))

addGreatestCheck :: Block -> Block
addGreatestCheck block = createBlock [(If (IfStatement (GreaterThan (createIdentifierExpression "msg.value") (createIdentifierExpression "contractBalance")) (BlockStatement $ block) (Nothing)))]

ifValueGreaterThan :: Variable -> Variable -> ContractBodyElement -> ContractBodyElement
ifValueGreaterThan (var1) (var2) (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Just block))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [(If (IfStatement (GreaterThan (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))) (BlockStatement $ block) (Nothing)))])))
ifValueGreaterThan (var1) (var2) (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Nothing))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [(If (IfStatement (GreaterThan (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))) (BlockStatement $ createBlock []) (Nothing)))])))

ifValueLessThan :: Variable -> Variable -> ContractBodyElement -> ContractBodyElement
ifValueLessThan (var1) (var2) (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Just block))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [(If (IfStatement (LessThan (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))) (BlockStatement $ block) (Nothing)))])))
ifValueLessThan (var1) (var2) (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Nothing))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [(If (IfStatement (LessThan (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))) (BlockStatement $ createBlock []) (Nothing)))])))

ifValueGreaterThanEqualTo :: Variable -> Variable -> ContractBodyElement -> ContractBodyElement
ifValueGreaterThanEqualTo (var1) (var2) (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Just block))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [(If (IfStatement (GreaterThanEQualTo (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))) (BlockStatement $ block) (Nothing)))])))
ifValueGreaterThanEqualTo (var1) (var2) (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Nothing))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [(If (IfStatement (GreaterThanEQualTo (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))) (BlockStatement $ createBlock []) (Nothing)))])))

ifValueLessThanEqualTo :: Variable -> Variable -> ContractBodyElement -> ContractBodyElement
ifValueLessThanEqualTo (var1) (var2) (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Just block))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [(If (IfStatement (LessthanEqualTo (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))) (BlockStatement $ block) (Nothing)))])))
ifValueLessThanEqualTo (var1) (var2) (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Nothing))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [(If (IfStatement (LessthanEqualTo (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))) (BlockStatement $ createBlock []) (Nothing)))])))

ifEqualTo :: Variable -> Variable -> ContractBodyElement -> ContractBodyElement
ifEqualTo (var1) (var2) (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Just block))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [(If (IfStatement (Equality (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))) (BlockStatement $ block) (Nothing)))])))
ifEqualTo (var1) (var2) (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Nothing))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [(If (IfStatement (Equality (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))) (BlockStatement $ createBlock []) (Nothing)))])))

ifNotEqualTo :: Variable -> Variable -> ContractBodyElement -> ContractBodyElement
ifNotEqualTo (var1) (var2) (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Just block))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [(If (IfStatement (InEquality (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))) (BlockStatement $ block) (Nothing)))])))
ifNotEqualTo (var1) (var2) (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Nothing))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (createBlock [(If (IfStatement (InEquality (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))) (BlockStatement $ createBlock []) (Nothing)))])))

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

setInterestRate :: ContractBodyElement
setInterestRate = StateVariableElem $ StateVariableDeclaration (ElementaryType uint) [PublicState] (createIdentifier "interestRate") (Nothing)

setTaxRate :: ContractBodyElement
setTaxRate = StateVariableElem $ StateVariableDeclaration (ElementaryType uint) [PublicState] (createIdentifier "taxRate") (Nothing)

setDIRT :: ContractBodyElement
setDIRT = StateVariableElem $ StateVariableDeclaration (ElementaryType uint) [PublicState] (createIdentifier "DIRT") (Nothing)

setOwner :: ContractBodyElement
setOwner = StateVariableElem $ StateVariableDeclaration (ElementaryType addressPayable) [PublicState] (createIdentifier "owner") (Nothing)

setRecipient :: String -> ContractBodyElement
setRecipient name = StateVariableElem $ StateVariableDeclaration (ElementaryType addressPayable) [PublicState] (createIdentifier name) (Nothing)

setBalance :: ContractBodyElement
setBalance = StateVariableElem $ stateMappingDeclaration "balance" (createMappingFromType address (ElementaryType uint)) []

setShares :: ContractBodyElement
setShares = StateVariableElem $ stateMappingDeclaration "shares" (createMappingFromType address (ElementaryType uint)) []

setAddresses :: String -> ContractBodyElement
setAddresses name = StateVariableElem $ stateMappingDeclaration name (createMappingFromType address (ElementaryType uint)) []

setTotalShares :: ContractBodyElement
setTotalShares = StateVariableElem $ StateVariableDeclaration (ElementaryType uint) [PublicState] (createIdentifier "totalShares") (Nothing)

setUnSignedAmount :: String ->  ContractBodyElement
setUnSignedAmount name = StateVariableElem $ StateVariableDeclaration (ElementaryType uint) [PublicState] (createIdentifier name) (Nothing)

setSignedAmount :: String ->  ContractBodyElement
setSignedAmount name = StateVariableElem $ StateVariableDeclaration (ElementaryType int) [PublicState] (createIdentifier name) (Nothing)

setBooleanVariable :: String ->  ContractBodyElement
setBooleanVariable name = StateVariableElem $ StateVariableDeclaration (ElementaryType $ Bool) [PublicState] (createIdentifier name) (Nothing)

loop :: Int -> ContractBodyElement -> ContractBodyElement
loop x (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Just block))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (addBlockToLoop (createLoop x) (block))))
loop x (FunctionElem (FunctionaDefinition name (params) modifiers (Nothing) (Nothing))) = (FunctionElem $ FunctionaDefinition name (params) modifiers (Nothing) (Just (addBlockToLoop (createLoop x) (createBlock []))))

contractBalanceAssignment = expressionToStatement $ Equals (createIdentifierExpression "contractBalance") (createIdentifierExpression "0")
ownerAssignment = expressionToStatement $ Equals (createIdentifierExpression "owner") (createIdentifierExpression "msg.sender")

buildConstructor :: [ContractBodyElement] -> ContractBodyElement
buildConstructor ((StateVariableElem (StateVariableDeclaration (ElementaryType addressPayable) [PublicState] (Identifier 'o' ['w', 'n', 'e', 'r']) (Nothing))):xs) = joinConstructors (ConstructElem (Constructor (Nothing) (Nothing) (createBlock [ownerAssignment]))) (buildConstructor xs)
buildConstructor ((StateVariableElem (StateVariableDeclaration (ElementaryType uint) [PublicState] (Identifier 'c' ['o', 'n', 't', 'r', 'a', 'c', 't', 'B', 'a', 'l', 'a', 'n', 'c', 'e']) (Nothing))):xs) = joinConstructors (ConstructElem (Constructor (Nothing) (Nothing) (createBlock [contractBalanceAssignment]))) (buildConstructor xs)
buildConstructor ((StateVariableElem (StateVariableDeclaration (ElementaryType varType) [PublicState] (Identifier y ys) (Nothing))):xs) = joinConstructors (ConstructElem (Constructor (Just $ createParameterList [(varType, (['_'] ++ y:ys))]) (Nothing) (createBlock [(expressionToStatement $ Equals (createIdentifierExpression (y:ys)) (createIdentifierExpression (['_'] ++[y] ++ ys)))]))) (buildConstructor xs)
buildConstructor (_:xs) = buildConstructor xs
buildConstructor [] = ConstructElem (Constructor (Nothing) (Nothing) (createBlock []))

createLoop :: Int -> Statement
createLoop x = For (ForStatement (VarInitialse (intVariableAssignmentDeclaration "i" (createIdentifierExpression "0"))) (Just $ ExpressionStatement (LessThan (createIdentifierExpression "i") (createIdentifierExpression (show x)))) (Just (PostIncrement (createIdentifierExpression "i"))) (BlockStatement $ createBlock []))

addBlockToLoop :: Statement -> Block -> Block
addBlockToLoop (For (ForStatement initialise (exprStatement) (expr) oldBlock)) block = createBlock [For $ (ForStatement initialise (exprStatement) (expr) (BlockStatement block))]

requireNotOwner = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (InEquality (createIdentifierExpression "owner") (createIdentifierExpression "msg.sender")) [])
requireOwner = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (Equality (createIdentifierExpression "owner") (createIdentifierExpression "msg.sender")) [])

requireRecipient :: Recipient -> Statement
requireRecipient (Recipient name) = expressionToStatement $ ExpressionArgs (createIdentifierExpression "require") (CallArgumentExpr (Equality (createIdentifierExpression name) (createIdentifierExpression "msg.sender")) [])

giveOwnership :: String -> ContractBodyElement
giveOwnership name = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Just $ createParameterList [(addressPayable, "newOwner")]) [VisibilityModifier PublicVisibility] (Nothing) (Just $ createBlock [requireOwner, (expressionToStatement $ Equals (createIdentifierExpression "owner") (createIdentifierExpression "newOwner"))])

getBalance :: String -> ContractBodyElement
getBalance name = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Nothing) [VisibilityModifier PublicVisibility, StateMutabilityModifier View] (Just $ ParameterList (ElementaryType uint) (Nothing) (Nothing) (Nothing)) (Just $ createBlock [(Return (ReturnStatement (Just ((Index (createIdentifierExpression "balance") (createIdentifierExpression "msg.sender"))))))])

getVariable :: String -> Variable -> ContractBodyElement
getVariable functionName (variable) = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier functionName)) (Nothing) [VisibilityModifier PublicVisibility, StateMutabilityModifier View] (Just $ ParameterList (getType variable) (Nothing) (Nothing) (Nothing)) (Just $ createBlock [(Return (ReturnStatement (Just (createIdentifierExpression (getVariableName variable)))))])

updateElement :: String -> Variable -> Variable -> ContractBodyElement
updateElement name var1 var2 = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier name)) (Nothing) [VisibilityModifier PublicVisibility, StateMutabilityModifier View] (Nothing) (Just $ createBlock [expressionToStatement $ Equals (createIdentifierExpression (getVariableName var1)) (createIdentifierExpression (getVariableName var2))])

becomeRecipient :: String -> Recipient -> ContractBodyElement
becomeRecipient functionName (Recipient name) = FunctionElem $ FunctionaDefinition (IdentifierName (createIdentifier (functionName))) (Nothing) [VisibilityModifier PublicVisibility] (Nothing) (Just $ createBlock [(expressionToStatement $ Equals (createIdentifierExpression name) (createIdentifierExpression "msg.sender"))])

proportionalTo :: Rate -> ContractBodyElement -> ContractBodyElement
proportionalTo (Shares) x = addRequirement x (expressionToStatement $ DivEquals (createIdentifierExpression "amount") (Div (Index (createIdentifierExpression "shares") (createIdentifierExpression "_to")) (createIdentifierExpression "totalShares")))
proportionalTo (InterestRate) x = addRequirement x (expressionToStatement $ Equals (createIdentifierExpression "amount") (Mul (Index (createIdentifierExpression "balance") (createIdentifierExpression "_to")) (createIdentifierExpression "interestRate")))
proportionalTo (TaxRate) x = addRequirement x (expressionToStatement $ Equals (createIdentifierExpression "amount") (Mul (Index (createIdentifierExpression "balance") (createIdentifierExpression "_to")) (createIdentifierExpression "taxRate")))
proportionalTo (InterestRateLessDIRT) x = addRequirement x (expressionToStatement $ Equals (createIdentifierExpression "amount") (Mul ((Mul (Index (createIdentifierExpression "balance") (createIdentifierExpression "_to")) (createIdentifierExpression "interestRate"))) (Sub (createIdentifierExpression "(1") (createIdentifierExpression "DIRT)")))  )

join :: String -> ContractBodyElement -> ContractBodyElement -> ContractBodyElement
join newName (FunctionElem (FunctionaDefinition name1 (params1) modifiers1 (Nothing) (Just block1))) (FunctionElem (FunctionaDefinition name2 (params2) modifiers2 (Nothing) (Just block2))) = (FunctionElem (FunctionaDefinition (IdentifierName $ createIdentifier newName) (joinParamLists params1 params2) (removeDuplicateVisibility $ joinModifiers modifiers1 modifiers2) (Nothing) (Just (joinBlocks block1 block2))))
join newName (FunctionElem (FunctionaDefinition name1 (params1) modifiers1 (Nothing) (Nothing))) (FunctionElem (FunctionaDefinition name2 (params2) modifiers2 (Nothing) (Nothing))) = (FunctionElem (FunctionaDefinition (IdentifierName $ createIdentifier newName) (joinParamLists params1 params2) (removeDuplicateVisibility $ joinModifiers modifiers1 modifiers2) (Nothing) (Nothing)))
join newName (FunctionElem (FunctionaDefinition name1 (params1) modifiers1 (Nothing) (Just block1))) (FunctionElem (FunctionaDefinition name2 (params2) modifiers2 (Nothing) (Nothing))) = (FunctionElem (FunctionaDefinition (IdentifierName $ createIdentifier newName) (joinParamLists params1 params2) (removeDuplicateVisibility $ joinModifiers modifiers1 modifiers2) (Nothing) (Just (block1))))
join newName (FunctionElem (FunctionaDefinition name1 (params1) modifiers1 (Nothing) (Nothing))) (FunctionElem (FunctionaDefinition name2 (params2) modifiers2 (Nothing) (Just block2))) = (FunctionElem (FunctionaDefinition (IdentifierName $ createIdentifier newName) (joinParamLists params1 params2) (removeDuplicateVisibility $ joinModifiers modifiers1 modifiers2) (Nothing) (Just (block2))))

getVariableName :: Variable -> String
getVariableName Owner = "owner"
getVariableName ContractBal = "contractBalance"
getVariableName StartTime = "start"
getVariableName EndTime = "end"
getVariableName (RecipientAddress (Recipient name)) = name
getVariableName Balances = "balance"
getVariableName (Addresses name) = name
getVariableName (UnSignedAmount name) = name
getVariableName (SignedAmount name) = name
getVariableName (BooleanVariable name) = name
getVariableName Interest = "interest"
getVariableName Tax = "taxRate"
getVariableName DIRT = "DIRT"
getVariableName SharesOwned = "shares"
getVariableName TotalShares = "totalShares"
getVariableName MessageValue = "msg.value"
getVariableName BoolTrue = "true"
getVariableName BoolFalse = "false"

getType :: Variable -> TypeName
getType Owner = ElementaryType uint
getType ContractBal = ElementaryType uint
getType StartTime = ElementaryType uint
getType EndTime = ElementaryType uint
getType (RecipientAddress _) = ElementaryType addressPayable
getType Balances = Mapping $ createMappingFromType address (ElementaryType uint)
getType (Addresses _) = Mapping $ createMappingFromType address (ElementaryType uint)
getType (SignedAmount _) = ElementaryType int
getType (UnSignedAmount _) = ElementaryType uint
getType (BooleanVariable _) = ElementaryType $ Bool
getType Interest = ElementaryType uint
getType Tax = ElementaryType uint
getType DIRT = ElementaryType uint
getType SharesOwned = Mapping $ createMappingFromType address (ElementaryType uint)
getType TotalShares = ElementaryType uint
-- getType MessageValue a message should not be able to return message value
getType BoolTrue = ElementaryType $ Bool
getType BoolFalse = ElementaryType $ Bool
