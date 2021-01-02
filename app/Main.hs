module Main where

import Lib
import Contracts
import SolidityPrinting
import SolidityAbstractSyntax
import System.IO

minter = SolidityVariable "minter"
address = SolidityDeclaration SolidityAddress "minter"
balances = SolidityVariable "balances"
balMap = SolidityDeclaration (SolidityMapping SolidityAddress SolidityUInt) "balances"
receiverV = SolidityVariable "receiver"
receiverDeclaration = SolidityDeclaration SolidityAddress "receiver"
amountV = SolidityVariable "amount"
amountDeclaration = SolidityDeclaration SolidityUInt "amount"
constructor = SolidityDeclaration (FunctionType (SolidityFunction Void [] [SAssign (SolidityLiteral "minter") (SolidityLiteral "msg.sender")])) "Coin"
inequalityCheck = SIf (If (Inequality (B $ SolidityLiteral "msg.sender") (B $ V minter)) [SReturn])
mint = SolidityDeclaration (FunctionType (SolidityFunction Void [receiverDeclaration, amountDeclaration] [inequalityCheck, SAssign (V balances) (Plus (V balances) (V amountV))])) "mint"
coin = Contract "Coin" [address, balMap, constructor, mint]

coinExample :: IO ()
coinExample = do
  outputContract coin "ContractOutputs/Coin.sol" solidityVersion

main :: IO ()
main = do
  coinExample
