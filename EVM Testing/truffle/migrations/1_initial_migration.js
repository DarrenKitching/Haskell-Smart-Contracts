const Migrations = artifacts.require("./Migrations");
const Bank = artifacts.require('./Bank.sol');
const ShareHolders = artifacts.require('./ShareHolders.sol');
const Interest = artifacts.require('./Interest.sol');
const RichestGame = artifacts.require('./RichestGame.sol');
const Taxes = artifacts.require('./Taxes.sol');
const Auction = artifacts.require('./Auction.sol');

module.exports = function (deployer) {
  deployer.deploy(Migrations);
  deployer.deploy(Bank);
  deployer.deploy(ShareHolders, 200);
  deployer.deploy(Interest);
  deployer.deploy(RichestGame);
  deployer.deploy(Taxes);
  deployer.deploy(Auction);
};
