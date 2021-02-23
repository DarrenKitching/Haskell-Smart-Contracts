module FinancialExamples where
import FinancialContracts

-- setContractBalance, setOwner, setStartTime, setEndTime, setReceipient, setBalances, setAddresses ____
-- ifOwner, ifBeforeEnd, ifAfterStart, ifBetweenDates
-- proportionalTo
-- Withdraw, Deposit, Transfer, GiveOwnership, Blank
-- All, SpecificAmount
-- ContractBalance, UserBalance
-- loop x


-- bank --
withdrawFromBalance = createFunction "withdraw" Withdraw SpecificAmount UserBalance
depositFromBalance = createFunction "deposit" Deposit All UserBalance
transferFromBalance = createFunction "transfer" Transfer SpecificAmount UserBalance
finance = createContract "Finance" [setBalance, setOwner, withdrawFromBalance, depositFromBalance, transferFromBalance]
-- bank --

-- shareholders --
withdrawFromDividends = createFunction "withdraw" Withdraw All UserBalance-- allows user to withdraw all of their dividends that has accumulated
depositToContract = ifOwner $ createFunction "depositToContract" Deposit All ContractBalance
payDividends = ifOwner $ proportionalTo "shares" $ createFunction "payDividends" Transfer SpecificAmount ContractBalance -- the owner can pay shares to an address and it will be assigned an amount based on its ownership
shareholders = createContract "ShareHolders" [setContractBalance, setOwner, setAddresses "shares", setBalance, withdrawFromDividends, payDividends, depositToContract]
-- shareholders --
