const ShareHolders = artifacts.require("ShareHolders");

/*
 * uncomment accounts to access the test accounts made available by the
 * Ethereum client
 * See docs: https://www.trufflesuite.com/docs/truffle/testing/writing-tests-in-javascript
 */
contract("ShareHolders", function (accounts) {
    const mary = accounts[0];
    const joe = accounts[1];
    const bill = accounts[2];
    const julie = accounts[3];
    it("Put 5 Wei into Mary's account", function() {
      return ShareHolders.deployed().then(function(instance) {
        instance.depositToContract({value: 5});
        instance.payDividends(mary, 200, {value: 200})
        return instance.getBalance.call(mary);
      }).then(function(balance) {
        assert.equal(balance.valueOf(), 5, "Mary didn't have a balance of 5");
      });
    });
    return assert.isTrue(true);
});