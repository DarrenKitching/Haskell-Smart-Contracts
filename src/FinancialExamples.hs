module FinancialExamples where
import FinancialContracts

-- setContractBalance, setOwner, setStartTime, setEndTime,
-- setReceipient, setBalances, setAddresses ____?, setInterstRate, setTaxRate, setDIRT, setShares
-- giveOwnership, getBalance*
-- ifOwner, ifBeforeEnd, ifAfterStart, ifBetweenDates
-- proportionalTo: InterstRate*, TaxRate*, DIRT?, Shares*
-- Withdraw, Deposit, Transfer, Blank()
-- All, SpecificAmount
-- ContractBalance, UserBalance
-- loop x


-- bank --
withdrawFromBalance = createFunction "withdraw" Withdraw SpecificAmount UserBalance
depositFromBalance = createFunction "deposit" Deposit All UserBalance
transferFromBalance = createFunction "transfer" Transfer SpecificAmount UserBalance
bank = createContract "Bank" [setBalance, setOwner, withdrawFromBalance, depositFromBalance, transferFromBalance]
-- bank --

-- shareholders --
withdrawFromDividends = createFunction "withdraw" Withdraw All UserBalance-- allows user to withdraw all of their dividends that has accumulated
depositToContract = ifOwner $ createFunction "depositToContract" Deposit All ContractBalance -- contract owner (company) can deposit to the contract
payDividends = ifOwner $ proportionalTo Shares $ createFunction "payDividends" Transfer SpecificAmount ContractBalance -- the owner can pay shares to an address and it will be assigned an amount based on its ownership
shareholders = createContract "ShareHolders" [setContractBalance, setOwner, setShares, setBalance, withdrawFromDividends, payDividends, depositToContract]
-- shareholders --

-- pay interest to customers --
customerWithdrawal = createFunction "customerWithdrawal" Withdraw SpecificAmount UserBalance
customerDeposit = createFunction "customerDeposit" Deposit All UserBalance
payInterest = proportionalTo InterestRate $ createFunction "payInterest" Transfer SpecificAmount ContractBalance
interest = createContract "Interest" [setOwner, setContractBalance, setBalance, setInterestRate, customerWithdrawal, customerDeposit, payInterest, giveOwnership]
-- pay intereset to customers --

-- collect taxes --
withdraw = createFunction "customerWithdrawal" Withdraw SpecificAmount UserBalance
deposit = createFunction "customerDeposit" Deposit All UserBalance
collectTaxes = proportionalTo TaxRate $ createFunction "payTaxes" Transfer SpecificAmount UserBalance
taxes = createContract "Taxes" [setOwner, setContractBalance, setBalance, setTaxRate, withdraw, deposit, collectTaxes]
-- collect taxes --
