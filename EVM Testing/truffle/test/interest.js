const Interest = artifacts.require("Interest");

/*
 * uncomment accounts to access the test accounts made available by the
 * Ethereum client
 * See docs: https://www.trufflesuite.com/docs/truffle/testing/writing-tests-in-javascript
 */
contract("Interest", function (accounts) {
  const mary = accounts[0];
  const joe = accounts[1];
  const bill = accounts[2];
  const julie = accounts[3];
  it("Pay interest to Mary", function() {
    return Interest.deployed().then(function(instance) { // interest rate = 2%, DIRT rate = 12%
      instance.customerDeposit({from: mary, value: 1000}); // put 1000 into Mary's account
      instance.depositToContract({value: 100}); // company deposits 100 to the contract so it can give people interest
      instance.payInterest(mary, 100); // amount called with makes no difference as amount is determined by balance of mary
      return instance.getBalance.call({from: mary});
    }).then(function(balance) {
      assert.equal(balance.valueOf().toNumber(), 1020, "Mary didn't have a balance of 1020");
    });
  });
  it("Pay interest less DIRT to Joe", function() {
    return Interest.deployed().then(function(instance) { // interest rate = 2%, DIRT rate = 12%
      instance.customerDeposit({from: joe, value: 1000}); // put 1000 into joe's account
      instance.depositToContract({value: 100}); // company deposits 100 to the contract so it can give people interest
      instance.payInterestLessDIRT(joe, 100); // amount called with makes no difference as amount is determined by balance of joe
      return instance.getBalance.call({from: joe});
    }).then(function(balance) {
      assert.equal(balance.valueOf().toNumber(), 1017, "Joe didn't have a balance of 1017");
    });
  });
  return assert.isTrue(true);
});
