module Main where

import Lib
import LanguageGrammarPrinting
import LanguageGrammar
import Abstractions
import ContractExamples
import FinancialContracts
import FinancialExamples

main :: IO ()
main = do
  -- outputSolidityContract is printing contracts defined in the Solidity Langugage Grammar. outputContract is print contracts defined in my DSL
  outputSolidityContract coinExample "ContractOutputs/Test.sol"
  outputSolidityContract shorternedCoinExample "ContractOutputs/AbstractionTest.sol"
  outputSolidityContract ballotExample "ContractOutputs/Ballot.sol"
  outputContract bank "ContractOutputs/Bank.sol"
  outputContract shareholders "ContractOutputs/ShareHolders.sol"
  outputContract interest "ContractOutputs/Interest.sol"
  outputContract taxes "ContractOutputs/Taxes.sol"
  outputContract richestGame "ContractOutputs/RichestGame.sol"
  outputContract auction "ContractOutputs/Auction.sol"

outputSolidityContract :: Contract -> String -> IO ()
outputSolidityContract contract destination = do
  writeFile destination ""
  appendFile destination "// SPDX-License-Identifier: GPL-3.0\n"
  appendFile destination (printSolidity contract)
