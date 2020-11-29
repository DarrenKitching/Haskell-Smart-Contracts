module Main where

import Lib
import Contracts
import SolidityGeneration
import System.IO

invoice = Invoice 1200 "0xE0f5206BBD039e7b0592d8918820024e2a7437b9"
stringExample = SolidityString "value"
intExample = SolidityInt "myInteger"
uIntExample = SolidityUInt "myUnsignedInteger"
boolExample = SolidityBool "myBool"
bytesExample = SolidityBytes "myBytes"
voterStruct = SolidityStruct "Voter" [SolidityInt "voterID", SolidityString "voterName"]
pollingStationStruct = SolidityStruct "PollingStation" [SolidityInt "pollingStationID", SolidityString "voterAddress"]

main :: IO ()
main = do
  produceInvoiceContract invoice "ContractOutputs/FullContract.sol"
  outputContract [stringExample, intExample, uIntExample, boolExample, bytesExample] [voterStruct, pollingStationStruct] [] "VarExample" "ContractOutputs/VarGeneration.sol" solidityVersion
