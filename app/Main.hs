module Main where

import Lib
import Contracts
import SolidityGeneration
import System.IO

invoice = Invoice 1200 "0xd925d24f99b01e0e0202ad2bcb9e2f5a7e7d409b6a23aedf6c8d5d784d2a0694"
stringExample = SolidityString "value"

main :: IO ()
main = do
  produceInvoiceContract invoice "ContractOutputs/FullContract.sol"
  putStrLn $ createVariable stringExample
  putStrLn $ createGetter stringExample
  putStrLn $ createSetter stringExample
