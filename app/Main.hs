module Main where

import Lib
import LanguageGrammarPrinting
import LanguageGrammar

version = (Pragma (PragmaToken 's') [(PragmaToken 'o'), (PragmaToken 'l'), (PragmaToken 'i'), (PragmaToken 'd'), (PragmaToken 'i'), (PragmaToken 't'), (PragmaToken 'y'), (PragmaToken ' '), (PragmaToken '^'), (PragmaToken '0'), (PragmaToken '.'), (PragmaToken '7'), (PragmaToken '.'), (PragmaToken '4')] contractDef)
contractDef = (ContractDef (ContractDefinition (Nothing) (Identifier 'S' ['i', 'm', 'p', 'l', 'e', 'S', 't', 'o','r', 'a', 'g', 'e']) (Nothing) [storedData, setFunc, getFunc]) EOF)
storedData = (StateVariableElem (StateVariableDeclaration (ElementaryType $ UnsignedIntType UInt) [] (Identifier 's' ['t', 'o', 'r', 'e', 'd', 'D', 'a', 't','a']) (Nothing)))

setParam = ParameterList (ElementaryType $ UnsignedIntType UInt) (Nothing) (Just (Identifier 'x' [])) []
setBlock = Block (Nothing) (Just (ExprStatement (ExpressionStatement (Equals (IdentifierExpr (Identifier 's' ['t', 'o', 'r', 'e', 'd', 'D', 'a', 't','a'])) (IdentifierExpr (Identifier 'x' [])))))) (Nothing)
setFunc = (FunctionElem (FunctionaDefinition (IdentifierName (Identifier 's' ['e', 't'])) (Just setParam) [(VisibilityModifier PublicVisibility)] (Nothing) (Just setBlock)))

getParam = ParameterList (ElementaryType $ UnsignedIntType UInt) (Nothing) (Nothing) []
getBlock = Block (Nothing) (Just (Return (ReturnStatement (Just (IdentifierExpr (Identifier 's' ['t', 'o', 'r', 'e', 'd', 'D', 'a', 't','a'])))))) (Nothing)
getFunc = (FunctionElem (FunctionaDefinition (IdentifierName (Identifier 'g' ['e', 't'])) (Nothing) [(VisibilityModifier PublicVisibility), (StateMutabilityModifier View)] (Just (getParam)) (Just getBlock)))

main :: IO ()
main = do
  outputContract version "ContractOutputs/test.sol"

outputContract :: Solidity -> String -> IO ()
outputContract contract destination = do
  writeFile destination ""
  appendFile destination "// SPDX-License-Identifier: GPL-3.0\n"
  appendFile destination (printSolidity contract)
