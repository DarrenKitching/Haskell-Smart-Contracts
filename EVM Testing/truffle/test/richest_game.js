const RichestGame = artifacts.require("RichestGame");

contract("RichestGame", function (accounts) {
  const mary = accounts[0];
  const joe = accounts[1];
  const bill = accounts[2];
  const julie = accounts[3];
  it("Play the Richest Game", function() {
    return RichestGame.deployed().then(function(instance) {
      instance.attemptToBecomeRichest({from: mary, value: 1000});
      instance.attemptToBecomeRichest({from: joe, value: 600});
      return instance.checkRichest.call({from: mary});
    }).then(function(richestPerson) {
      assert.equal(richestPerson, mary, "Mary was not the richest person.");
    });
  });
  it("Play the Richest Game", function() {
    return RichestGame.deployed().then(function(instance) {
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
