const Interest = artifacts.require("Interest");

contract("Interest", function (accounts) {
  const mary = accounts[0];
  const joe = accounts[1];
  const bill = accounts[2];
  const julie = accounts[3];
  it("Pay interest to Mary", function() {
    return Interest.deployed().then(function(instance) {
      instance.customerDeposit({from: mary, value: 1000});
      instance.depositToContract({value: 100});
      instance.payInterest(mary, 100);
      return instance.getBalance.call({from: mary});
    }).then(function(balance) {
      assert.equal(balance.valueOf().toNumber(), 1020,
        "Mary didn't have a balance of 1020");
    });
  });
  it("Pay interest less DIRT to Joe", function() {
    return Interest.deployed().then(function(instance) {
      instance.customerDeposit({from: joe, value: 1000});
      instance.depositToContract({value: 100});
      instance.payInterestLessDIRT(joe, 100);
      return instance.getBalance.call({from: joe});
    }).then(function(balance) {
      assert.equal(balance.valueOf().toNumber(), 1017,
        "Joe didn't have a balance of 1017");
    });
  });
  return assert.isTrue(true);
});
