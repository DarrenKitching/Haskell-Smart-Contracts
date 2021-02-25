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
  outputContract coinExample "ContractOutputs/Test.sol"
  outputContract shorternedCoinExample "ContractOutputs/AbstractionTest.sol"
  outputContract ballotExample "ContractOutputs/Ballot.sol"
  outputContract bank "ContractOutputs/Bank.sol"
  outputContract shareholders "ContractOutputs/ShareHolders.sol"
  outputContract interest "ContractOutputs/Interest.sol"
  outputContract taxes "ContractOutputs/Taxes.sol"

outputContract :: Contract -> String -> IO ()
outputContract contract destination = do
  writeFile destination ""
  appendFile destination "// SPDX-License-Identifier: GPL-3.0\n"
  appendFile destination (printSolidity contract)
