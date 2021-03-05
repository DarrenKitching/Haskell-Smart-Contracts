module FinancialExamples where
import FinancialContracts

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
-- shareholders --

-- pay interest to customers --
customerWithdrawal = CreateFinancialFunction "customerWithdrawal" Withdraw SpecificAmount UserBalance
customerDeposit = CreateFinancialFunction "customerDeposit" Deposit All UserBalance
payInterest = ProportionalTo InterestRate (CreateFinancialFunction "payInterest" Transfer SpecificAmount ContractBalance)
payInterestLessDIRT = ProportionalTo InterestRateLessDIRT (CreateFinancialFunction "payInterestLessDIRT" Transfer SpecificAmount ContractBalance)
transferOwnership = CreateCoreFunction "giveOwnership" GiveOwnership
interest = CreateContract "Interest" [Set Owner, Set ContractBal, Set Balances, Set Interest, Set DIRT, customerWithdrawal, customerDeposit, payInterest, payInterestLessDIRT, transferOwnership]
-- pay intereset to customers --

-- collect taxes --
withdraw = CreateFinancialFunction "customerWithdrawal" Withdraw SpecificAmount UserBalance
deposit = CreateFinancialFunction "customerDeposit" Deposit All UserBalance
collectTaxes = ProportionalTo TaxRate (CreateFinancialFunction "payTaxes" Transfer SpecificAmount UserBalance)
taxes = CreateContract "Taxes" [Set Owner, Set ContractBal, Set Balances, Set Tax, withdraw, deposit, collectTaxes]
-- collect taxes --

-- richest game --
richest = Recipient "richest"
depositToContractBal = CreateFinancialFunction "deposit" Deposit All ContractBalance
becomeRichest = Conditioned (RequireBetweenDates) (Conditioned (ValueGreaterThan (MessageValue) (ContractBal)) (CreateCoreFunction "becomeRichest" (BecomeRecipient (richest))))
attemptToBecomeRichest = Join "attemptToBecomeRichest" depositToContractBal becomeRichest
withdrawIfRichest = Conditioned (RequireEnded) (Conditioned (RequireRecipient richest) (CreateFinancialFunction "withdraw" Withdraw All ContractBalance))
richestGame = CreateContract "RichestGame" [Set (RecipientAddress (richest)), Set ContractBal, Set StartTime, Set EndTime, attemptToBecomeRichest, withdrawIfRichest]
-- richest game --

-- auction --
winner = Recipient "winner"
highestBid = UnSignedAmount "highestBid"
newBid = UnSignedAmount "newBid"
payIfWinner = Conditioned (RequireEnded) (Conditioned (RequireRecipient winner) ((Conditioned (EqualTo (highestBid) (MessageValue)) (CreateFinancialFunction "payIfWinner" Deposit All ContractBalance))))
bid = Conditioned (RequireBetweenDates) (Conditioned (ValueGreaterThan (newBid) (highestBid)) (Join ("bid") (CreateCoreFunction "becomeWinner" (BecomeRecipient (winner))) (CreateCoreFunction "updateHighestBid" (Update (highestBid) (newBid)))))
checkWinner = (CreateCoreFunction "checkWinner" (GetRecipient (winner)))
ownerWithdrawAfterAuctionEnded = Conditioned (RequireEnded) (Conditioned (RequireOwner) (CreateFinancialFunction "withdraw" Withdraw All ContractBalance))
auction = CreateContract "Auction" [Set (RecipientAddress (winner)), Set Owner, Set ContractBal, Set highestBid, Set StartTime, Set EndTime, ownerWithdrawAfterAuctionEnded, bid, payIfWinner, checkWinner]
-- auction --
