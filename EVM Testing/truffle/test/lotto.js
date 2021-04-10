const Lotto = artifacts.require("Lotto");

contract("Lotto", function (accounts) {
  console.log("Accounts visible: ", accounts);
  const mary = accounts[0];
  const joe = accounts[1];
  const bill = accounts[2];
  const julie = accounts[3];
  it("Lotto run with four participants", function() {
    return Lotto.deployed().then(function(instance) {
      instance.buyTicket(5, {from: mary, value: 5});                                                                  // mary buys a ticket
      instance.buyTicket(5, {from: joe, value: 5});                                                                   // joe buys a ticket
      instance.buyTicket(5, {from: bill, value: 5});                                                                  // bill buys a ticket
      instance.buyTicket(5, {from: julie, value: 5});                                                                  // julie buys a ticket
      instance.pickWinner();                                                                                          // contract owner calls pick winner function
      return instance.checkWinner.call();
    }).then(function(winner) {
      if(winner == mary) {
        assert.equal(winner, mary, "Mary was not the winner.");
      }
      else if (winner == joe) {
        assert.equal(winner, joe, "Joe was not the winner.");
      }
      else if (winner == bill) {
        assert.equal(winner, bill, "Bill was not the winner.");
      }
      else if (winner == julie) { // winner should be julie
        assert.equal(winner, julie, "Julie was not the winner.");
      }
      else {
        assert.equal(winner, mary,
          "Neither Mary, Joe, Bill nor Julie was the winner. Something went wrong!")
      }
    });
  });
  return assert.isTrue(true);
});
