const ShareHolders = artifacts.require("ShareHolders");

contract("ShareHolders", function (accounts) {
    const mary = accounts[0];
    const joe = accounts[1];
    const bill = accounts[2];
    const julie = accounts[3];
    it("Pay out Dividends to Mary", function() {
      return ShareHolders.deployed().then(function(instance) {
        instance.depositToContract({value: 1000});
        instance.setShares(mary, 10);
        instance.payDividends(mary, 200, {value: 200})
        return instance.getBalance.call({from: mary});
      }).then(function(balance) {
        assert.equal(balance.valueOf().toNumber(), 10,
          "Mary didn't have a balance of 5");
      });
    });
    return assert.isTrue(true);
});
