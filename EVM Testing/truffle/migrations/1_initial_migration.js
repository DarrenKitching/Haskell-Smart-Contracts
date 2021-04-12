const Migrations = artifacts.require("./Migrations");
const Bank = artifacts.require('./Bank.sol');
const ShareHolders = artifacts.require('./ShareHolders.sol');
const Interest = artifacts.require('./Interest.sol');
const RichestGame = artifacts.require('./RichestGame.sol');
const Taxes = artifacts.require('./Taxes.sol');
const Auction = artifacts.require('./Auction.sol');
const Lotto = artifacts.require('./Lotto.sol');

module.exports = function (deployer, network, accounts) {
  const test = accounts[5];
  deployer.deploy(Migrations);
  deployer.deploy(Bank);
  deployer.deploy(RichestGame, test, 3600, 0); // 3600 seconds (1 hour) game
  deployer.deploy(ShareHolders, 200);
  deployer.deploy(Interest, 2, 12);
  deployer.deploy(Taxes, 25);
  deployer.deploy(Auction, test, 0, false, 3600); // 3600 seconds (1 hour) auction
  deployer.deploy(Lotto, test, 0, 100, 5); // inital winner initialised as account[5]. Number Of Players 0, Max number of players 100. Ticket Price 5 Wei.
};
