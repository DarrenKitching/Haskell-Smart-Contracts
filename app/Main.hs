module Main where

import Lib
import Contracts
import SolidityPrinting
import SolidityAbstractSyntax
import System.IO

address = SolidityDeclaration SolidityAddress "minter"
balMap = SolidityDeclaration (SolidityMapping SolidityAddress SolidityUInt) "balances"
constructor = SolidityDeclaration (FunctionType (SolidityFunction Void [] [SAssign (SolidityLiteral "minter") (SolidityLiteral "msg.sender")])) "Coin"
coin = Contract "Coin" [address, balMap, constructor]


coinExample :: IO ()
coinExample = do
  outputContract coin "ContractOutputs/Coin.sol" solidityVersion

main :: IO ()
main = do
  coinExample
