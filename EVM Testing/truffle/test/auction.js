const Auction = artifacts.require("Auction");

/*
 * uncomment accounts to access the test accounts made available by the
 * Ethereum client
 * See docs: https://www.trufflesuite.com/docs/truffle/testing/writing-tests-in-javascript
 */
contract("Auction", function (accounts) {
    const mary = accounts[0];
    const joe = accounts[1];
    const bill = accounts[2];
    const julie = accounts[3];
    it("Auction test", function() { // initial winner = test, highestBid = 0, hasPaid = false, start = 0, end = 100);
      return Auction.deployed().then(function(instance) {
        instance.bid(1000, {from: mary});
        instance.bid(1100, {from: joe});
        instance.bid(500, {from: mary});
        instance.bid(1150, {from: bill});
        return instance.checkWinner.call();
      }).then(function(winner) {
        assert.equal(winner, bill, "Bill was not the winner.");
      });
    });
    return assert.isTrue(true);
});
