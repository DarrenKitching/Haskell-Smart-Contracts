const Taxes = artifacts.require("Taxes");

contract("Taxes", function (accounts) {
  const mary = accounts[0];
  const joe = accounts[1];
  const bill = accounts[2];
  const julie = accounts[3];
  const taxCollector = accounts[4];
  it("Collect taxes", function() {
    return Taxes.deployed().then(function(instance) {
      instance.customerDeposit({from: mary, value: 1000});
      instance.payTaxes(taxCollector, 0, {from: mary});
      return instance.getBalance.call({from: mary});
    }).then(function(balance) {
      assert.equal(balance.valueOf().toNumber(), 750,
        "Mary didn't have a balance of 750");
    });
  });
});
