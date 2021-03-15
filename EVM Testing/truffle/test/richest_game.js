const RichestGame = artifacts.require("RichestGame");

/*
 * uncomment accounts to access the test accounts made available by the
 * Ethereum client
 * See docs: https://www.trufflesuite.com/docs/truffle/testing/writing-tests-in-javascript
 */
contract("RichestGame", function (accounts) {
  const mary = accounts[0];
  const joe = accounts[1];
  const bill = accounts[2];
  const julie = accounts[3];
  it("Play the Richest Game", function() {
    return RichestGame.deployed().then(function(instance) { //inital richest = accounts[5], start = 0, end = 100, highestAmount = 0
      instance.attemptToBecomeRichest({from: mary, value: 1000});
      instance.attemptToBecomeRichest({from: joe, value: 600});
      return instance.checkRichest.call({from: mary});
    }).then(function(richestPerson) {
      assert.equal(richestPerson, mary, "Mary was not the richest person.");
    });
  });
  it("Play the Richest Game", function() {
    return RichestGame.deployed().then(function(instance) { //inital richest = accounts[5], start = 0, end = 100, highestAmount = 0
      instance.attemptToBecomeRichest({from: mary, value: 1000});
      instance.attemptToBecomeRichest({from: joe, value: 600});
      instance.attemptToBecomeRichest({from: bill, value: 3000});
      instance.attemptToBecomeRichest({from: julie, value: 2150});
      return instance.checkRichest.call();
    }).then(function(richestPerson) {
      assert.equal(richestPerson, bill, "Bill was not the richest person.");
    });
  });
  return assert.isTrue(true);
});
