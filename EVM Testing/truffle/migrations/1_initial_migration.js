const Migrations = artifacts.require("./Migrations");
const Bank = artifacts.require('./Bank.sol');
const ShareHolders = artifacts.require('./ShareHolders.sol');
const Interest = artifacts.require('./Interest.sol');
const RichestGame = artifacts.require('./RichestGame.sol');
const Taxes = artifacts.require('./Taxes.sol');
const Auction = artifacts.require('./Auction.sol');

module.exports = function (deployer, network, accounts) {
  const test = accounts[5];
  deployer.deploy(Migrations);
  deployer.deploy(Bank);
  deployer.deploy(RichestGame, test, 0, 1000000000, 0);
  deployer.deploy(ShareHolders, 200);
  deployer.deploy(Interest, 2, 12);
  deployer.deploy(Taxes, 25);
  deployer.deploy(Auction, test, 0, false, 0, 1000000000);
};
