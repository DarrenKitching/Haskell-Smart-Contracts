const Interest = artifacts.require("Interest");

/*
 * uncomment accounts to access the test accounts made available by the
 * Ethereum client
 * See docs: https://www.trufflesuite.com/docs/truffle/testing/writing-tests-in-javascript
 */
contract("Interest", function (/* accounts */) {
  it("should assert true", async function () {
    await Interest.deployed();
    return assert.isTrue(true);
  });
});
