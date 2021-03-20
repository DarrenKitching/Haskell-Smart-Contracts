const Bank = artifacts.require("Bank");

/*
 * uncomment accounts to access the test accounts made available by the
 * Ethereum client
 * See docs: https://www.trufflesuite.com/docs/truffle/testing/writing-tests-in-javascript
 */
contract("Bank", function (accounts) {
    console.log("Accounts visible: ", accounts);
    const mary = accounts[0];         // mary wants to deposit 5 Wei
    const joe = accounts[1];          // joe wants to deposit 10 Wei and then withdrwas 7 Wei
    const bill = accounts[2];         // bill wants to transfer 3 Wei from his account to Mary's
    const julie = accounts[3];
    it("Put 5 Wei into Mary's account", function() {
      return Bank.deployed().then(function(instance) {
        instance.deposit({from: mary, value: 5});
        return instance.getBalance.call({from: mary});
      }).then(function(balance) {
        assert.equal(balance.valueOf(), 5, "Mary didn't have a balance of 5");
      });
    });
    it("Joe deposits 10 Wei and then withdraws 7 Wei", function() {
      return Bank.deployed().then(function(instance) {
        instance.deposit({from: joe, value: 10});
        instance.withdraw(7, {from: joe});
        return instance.getBalance.call({from: joe});
      }).then(function(balance) {
        assert.equal(balance.valueOf(), 3, "Joe didn't have a balance of 3");
      });
    });
    it("Bill deposits 15 Wei. He transfer 5 Wei to Julie. Bill's balance should now be 10 and Julie's balance should be 5.", function() {
      return Bank.deployed().then(function(instance) {
        instance.deposit({from: bill, value: 15});
        instance.transfer(julie, 5, {from: bill});
        //return instance.getBalance.call({from: bill});
        return instance.getBalance.call({from: bill});
      }).then(function(balance) {
        assert.equal(balance.valueOf(), 10, "Bill didn't have a balance of 10");
      });
    });
    return assert.isTrue(true);
});
