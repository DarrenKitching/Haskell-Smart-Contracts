module Contracts where
import SolidityPrinting
import SolidityAbstractSyntax
import System.IO

solidityVersion = "pragma solidity >=0.4.22 <0.8.0;"

-- minter contract --
minter = SolidityVariable "minter"
balances = SolidityVariable "balances"
receiverV = SolidityVariable "receiver"
amountV = SolidityVariable "amount"

-- declarations --
address = SolidityDeclaration SolidityAddress "minter"
balMap = SolidityDeclaration (SolidityMapping SolidityAddress SolidityUInt) "balances"
receiverDeclaration = SolidityDeclaration SolidityAddress "receiver"
amountDeclaration = SolidityDeclaration SolidityUInt "amount"
constructor = SolidityDeclaration constructorFunction "Coin"
mint = SolidityDeclaration mintFunction "mint"
send = SolidityDeclaration sendFunction "send"

-- bool expressions --
inequalityCheck = SIf (If (Inequality (B $ SolidityLiteral "msg.sender") (B $ V minter)) [SReturn])
lessThanCheck = SIf (If (LessThan (B $ V balances) (B $ V amountV)) [SReturn])

-- functions --
mintFunction = FunctionType (SolidityFunction Void [receiverDeclaration, amountDeclaration] [inequalityCheck, SAssign (VI (balances) (V receiverV)) (Plus (V balances) (V amountV))])
constructorFunction = FunctionType (SolidityFunction Void [] [SAssign (SolidityLiteral "minter") (SolidityLiteral "msg.sender")])
sendFunction = FunctionType (SolidityFunction Void [receiverDeclaration, amountDeclaration] [lessThanCheck, SAssign (VI balances (SolidityLiteral "msg.sender")) (Minus (V balances) (V amountV)), SAssign (VI balances (V receiverV)) (Plus (V balances) (V amountV))])

-- contract --
coin = Contract "Coin" [address, balMap, constructor, mint, send] -- contract takes list of declarations


coinExample :: IO ()
coinExample = do
  outputContract coin "ContractOutputs/Coin.sol" solidityVersion
