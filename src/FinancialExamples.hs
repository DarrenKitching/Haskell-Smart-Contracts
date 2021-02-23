module FinancialExamples where
import FinancialContracts

-- setContractBalance, setOwner, setStartTime, setEndTime, setReceipient, setBalance
-- ifOwner, ifBeforeEnd, ifAfterStart, ifBetweenDates
-- Withdraw, Deposit, Transfer, GiveOwnership, Blank
-- All, SpecificAmount
-- ContractBalance, UserBalance
-- loop x


-- bank --
withdrawFromBalance = createFunction "withdraw" Withdraw SpecificAmount UserBalance
depositFromBalance = createFunction "deposit" Deposit SpecificAmount ContractBalance
transferFromBalance = createFunction "transfer" Transfer SpecificAmount UserBalance
finance = createContract "Finance" [setContractBalance, setBalance "balance", setOwner, withdrawFromBalance, depositFromBalance, transferFromBalance]
-- bank --

-- shareholders --
-- withdrawFromDividends = createFunction "withdraw" Withdraw UserShare -- allows user to withdraw all of the dividends that have accumulated
-- payDividends = ifOwner $ proportionalTo "shares" $ createFunction "payDividends" Deposit SpecificAmount -- the owner can pay shares to an address and it will be assigned an amount based on its ownership
-- shareholders = createContract "ShareHolders" [setOwner, setBalance "shares", setBalance "balance", withdrawFromDividends]
-- shareholders --
