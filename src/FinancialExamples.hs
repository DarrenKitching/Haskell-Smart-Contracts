module FinancialExamples where
import FinancialContracts

-- bank --
withdrawFromBalance = FunctionElement "withdraw" (Financial Withdraw SpecificAmount UserBalance)
depositFromBalance = FunctionElement "deposit" (Financial Deposit All UserBalance)
transferFromBalance = FunctionElement "transfer" (Financial Transfer SpecificAmount UserBalance)
getBal = FunctionElement "getBalance" (Core GetBalance)
bank = Contract "Bank" [Set Balances, Set Owner,
  withdrawFromBalance, depositFromBalance, transferFromBalance, getBal]
-- bank --

-- shareholders --
shareHolder = Recipient "shareHolder"
numberOfShares = UnSignedAmount "sharesOwned"
withdrawFromDividends = FunctionElement "withdraw" (Financial Withdraw All UserBalance) -- allows user to withdraw all of their dividends that has accumulated
depositToContract = FunctionElement "depositToContract" (Conditioned (RequireOwner) (Financial Deposit All ContractBalance)) -- contract owner (company) can deposit to the contract
payDividends = FunctionElement "payDividends" (Conditioned (RequireOwner) (ProportionalTo (SignedAmount "amount") (Shares)
  (Financial Transfer SpecificAmount ContractBalance))) -- the owner can pay shares to an address and it will be assigned an amount based on its ownership
getBalanceAfterPaid = FunctionElement "getBalance" (Core GetBalance)
setSharesOwned = FunctionElement "setShares" (Conditioned (RequireOwner) (Core (Update (IndexAddress SharesOwned shareHolder) (numberOfShares) [RecipientAddress shareHolder, numberOfShares])))
shareholders = Contract "ShareHolders" [Set ContractBal, Set Owner,
  Set TotalShares, Set SharesOwned, Set Balances, withdrawFromDividends, payDividends, depositToContract, setSharesOwned, getBalanceAfterPaid]
-- shareholders --

-- pay interest to customers --
customerWithdrawal = FunctionElement "customerWithdrawal" (Financial Withdraw SpecificAmount UserBalance)
customerDeposit = FunctionElement "customerDeposit" (Financial Deposit All UserBalance)
payInterest = FunctionElement "payInterest" (ProportionalTo (SignedAmount "amount") InterestRate (Financial Transfer SpecificAmount ContractBalance))
payInterestLessDIRT = FunctionElement "payInterestLessDIRT" (ProportionalTo (SignedAmount "amount") InterestRateLessDIRT (Financial Transfer SpecificAmount ContractBalance))
transferOwnership = FunctionElement "giveOwnership" (Core GiveOwnership)
interest = Contract "Interest" [Set Owner, Set ContractBal, Set Balances, Set Interest, Set DIRT, customerWithdrawal, customerDeposit, payInterest, payInterestLessDIRT, transferOwnership]
-- pay intereset to customers --

-- collect taxes --
withdraw = FunctionElement "customerWithdrawal" (Financial Withdraw SpecificAmount UserBalance)
deposit = FunctionElement "customerDeposit" (Financial Deposit All UserBalance)
collectTaxes = FunctionElement "payTaxes" (ProportionalTo (SignedAmount "amount") TaxRate (Financial Transfer SpecificAmount UserBalance))
taxes = Contract "Taxes" [Set Owner, Set ContractBal, Set Balances, Set Tax, withdraw, deposit, collectTaxes]
-- collect taxes --

-- richest game --
richest = Recipient "richest"
highestAmount = UnSignedAmount "highestAmount"
depositToContractBal =Conditioned (RequireTime BetweenStartAndEnd) (Financial Deposit All ContractBalance)
becomeRichest = Conditioned (RequireVariableRelation ValueGreaterThan MessageValue highestAmount) (Join
  (Core (BecomeRecipient (richest))) (Core (Update (highestAmount) (MessageValue) [])))
attemptToBecomeRichest = FunctionElement "attemptToBecomeRichest" (Join depositToContractBal becomeRichest)
withdrawIfRichest = FunctionElement "withdraw" (Conditioned (RequireTime Ended) (Conditioned (RequireRecipient richest)
  (Financial Withdraw All ContractBalance)))
richestGame = Contract "RichestGame" [Set (RecipientAddress (richest)), Set ContractBal, Set StartTime,
  Set EndTime, Set highestAmount, attemptToBecomeRichest, withdrawIfRichest]
-- richest game --

-- auction --
winner = Recipient "winner"
highestBid = UnSignedAmount "highestBid"
newBid = UnSignedAmount "newBid"
hasPaid = BooleanVariable "hasPaid"
payIfWinner = FunctionElement "payIfWinner" (Conditioned (RequireTime Ended) (Conditioned (RequireRecipient winner) ((Conditioned (RequireVariableRelation ValueEqualTo (highestBid) (MessageValue)) (Join
  (Financial Deposit All ContractBalance) (Core (Update hasPaid BoolTrue [])))))))
bid = FunctionElement "bid" (Conditioned (RequireTime BetweenStartAndEnd) (Conditioned (RequireVariableRelation ValueGreaterThan newBid highestBid) (Join (Core (BecomeRecipient (winner))) (Core (Update (highestBid) (newBid) [newBid])))))
checkWinner = FunctionElement "checkWinner" (Core (GetVariable (RecipientAddress winner)))
ownerWithdrawAfterAuctionEnded = FunctionElement "withdraw" (Conditioned (RequireTime Ended) (Conditioned (RequireOwner) (Conditioned (RequireTrue (hasPaid))
  (Financial Withdraw All ContractBalance))))
auction = Contract "Auction" [Set (RecipientAddress (winner)), Set Owner, Set ContractBal, Set highestBid, Set hasPaid, Set StartTime, Set EndTime,
  ownerWithdrawAfterAuctionEnded, bid, payIfWinner, checkWinner]
-- auction --

-- test --
testTimes = SignedAmount "test"
wac = FunctionElement "LoopTest" (Loop (testTimes) (Financial Withdraw All ContractBalance))
test = Contract "Test" [Set ContractBal, Set testTimes, wac]
-- test --
