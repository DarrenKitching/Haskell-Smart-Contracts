module FinancialExamples where
import FinancialContracts

{-
data Language = CreateFinancialFunction String FinancialFunctionality Amount Source
              | CreateCoreFunction String CoreFunctionality
              | CreateContract String [Language] -- may want to split into declaration and statements
              | Set SetElement -- might just have setOwner, setBalance etc.
              | ProportionalTo Rate Language -- throw all into one data structure to begin with then see how to break it
              | Loop Int Language
              | Conditioned Condition Language

data Condition = RequireOwner | RequireStarted | RequiredNotEnded | RequireBetweenDates
data SetElement = Owner | ContractBal | StartTime | EndTime | Receipient String | Balances | Addresses String | Interest | Tax | DIRT | SharesOwned | TotalShares
data CoreFunctionality = GiveOwnership | GetBalance
data Amount = All | SpecificAmount
data Source = ContractBalance | UserBalance
data FinancialFunctionality = Withdraw | Deposit | Transfer | Blank
data Rate = InterestRate | TaxRate | Shares | DIRTRate
-}

-- bank --
withdrawFromBalance = CreateFinancialFunction "withdraw" Withdraw SpecificAmount UserBalance
depositFromBalance = CreateFinancialFunction "deposit" Deposit All UserBalance
transferFromBalance = CreateFinancialFunction "transfer" Transfer SpecificAmount UserBalance
getBal = CreateCoreFunction "getBalance" GetBalance
bank = CreateContract "Bank" [Set Balances, Set Owner, withdrawFromBalance, depositFromBalance, transferFromBalance, getBal]
-- bank --

-- shareholders --
withdrawFromDividends = CreateFinancialFunction "withdraw" Withdraw All UserBalance-- allows user to withdraw all of their dividends that has accumulated
depositToContract = Conditioned (RequireOwner) (CreateFinancialFunction "depositToContract" Deposit All ContractBalance) -- contract owner (company) can deposit to the contract
payDividends = Conditioned (RequireOwner) (ProportionalTo (Shares) (CreateFinancialFunction "payDividends" Transfer SpecificAmount ContractBalance)) -- the owner can pay shares to an address and it will be assigned an amount based on its ownership
shareholders = CreateContract "ShareHolders" [Set ContractBal, Set Owner, Set TotalShares, Set SharesOwned, Set Balances, withdrawFromDividends, payDividends, depositToContract]

-- pay interest to customers --
customerWithdrawal = CreateFinancialFunction "customerWithdrawal" Withdraw SpecificAmount UserBalance
customerDeposit = CreateFinancialFunction "customerDeposit" Deposit All UserBalance
payInterest = ProportionalTo InterestRate (CreateFinancialFunction "payInterest" Transfer SpecificAmount ContractBalance)
transferOwnership = CreateCoreFunction "giveOwnership" GiveOwnership
interest = CreateContract "Interest" [Set Owner, Set ContractBal, Set Balances, Set Interest, customerWithdrawal, customerDeposit, payInterest, transferOwnership]
-- pay intereset to customers --

-- collect taxes --
withdraw = CreateFinancialFunction "customerWithdrawal" Withdraw SpecificAmount UserBalance
deposit = CreateFinancialFunction "customerDeposit" Deposit All UserBalance
collectTaxes = ProportionalTo TaxRate (CreateFinancialFunction "payTaxes" Transfer SpecificAmount UserBalance)
taxes = CreateContract "Taxes" [Set Owner, Set ContractBal, Set Balances, Set Tax, withdraw, deposit, collectTaxes]
-- collect taxes --
