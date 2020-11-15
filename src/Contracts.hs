module Contracts where

import System.IO

data Invoice amount receiver = Invoice { amount :: Integer
                                       , receiver :: String
                                       }
data EnumState = Created | Locked | Inactive
data FunctionState = External | Public | Internal | Private

class Contract c where
  value :: c -> Integer
  state :: c -> EnumState
  getReceiver :: c -> String
  prodceContract :: c -> String -> IO ()

solidityVersion = "pragma solidity >=0.4.22 <0.8.0;"

instance Contract (Invoice amount receiver) where
  value (Invoice amount _) = amount
  state _ = Created
  getReceiver (Invoice _ receiver) = receiver
  prodceContract c = produceInvoiceContract (c)

produceInvoiceContract :: (Invoice amount receiver) -> String -> IO ()
produceInvoiceContract (Invoice amount receiver) destination = do
  writeFile destination ""
  contents1 <- readFile "ContractComponents/FullContract1.txt"
  contents2 <- readFile "ContractComponents/FullContract2.txt"
  contents3 <- readFile "ContractComponents/FullContract3.txt"
  appendFile destination (contents1)
  appendFile destination ("\"" ++ receiver ++ "\"")
  appendFile destination (contents2)
  appendFile destination (show $ amount)
  appendFile destination (contents3)

{-
zero :: Contract

one :: Currency -> Contract

give :: Contract -> Contract

and :: Contract -> Contract -> Contract

or :: Contract -> Contract -> Contract

truncate :: Date -> Contract -> Contract

then :: Contract -> Contract -> Contract

scale :: Obs Double -> Contract -> Contract

get :: Contract -> Contract

anytime :: Contract -> Contract
-}
