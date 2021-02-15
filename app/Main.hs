module Main where

import Lib
import LanguageGrammarPrinting
import LanguageGrammar
import Abstractions
import ContractExamples

main :: IO ()
main = do
  outputContract coinExample "ContractOutputs/Test.sol"
  outputContract shorternedCoinExample "ContractOutputs/AbstractionTest.sol"
  outputContract ballotExample "ContractOutputs/Ballot.sol"

outputContract :: Solidity -> String -> IO ()
outputContract contract destination = do
  writeFile destination ""
  appendFile destination "// SPDX-License-Identifier: GPL-3.0\n"
  appendFile destination (printSolidity contract)
