const Taxes = artifacts.require("Taxes");

/*
 * uncomment accounts to access the test accounts made available by the
 * Ethereum client
 * See docs: https://www.trufflesuite.com/docs/truffle/testing/writing-tests-in-javascript
 */
contract("Taxes", function (accounts) {
  const mary = accounts[0];
  const joe = accounts[1];
  const bill = accounts[2];
  const julie = accounts[3];
  const taxCollector = accounts[4];
  it("Collect taxes", function() {
    return Taxes.deployed().then(function(instance) { //tax rate = 25%
      instance.customerDeposit({from: mary, value: 1000}); // mary deposits 1000 to her account
      instance.payTaxes(taxCollector, 0, {from: mary}); // amount called with has no effect as amount is set in accordance with Mary's balance
      return instance.getBalance.call({from: mary});
    }).then(function(balance) {
      assert.equal(balance.valueOf().toNumber(), 750, "Mary didn't have a balance of 750");
    });
  });
});
