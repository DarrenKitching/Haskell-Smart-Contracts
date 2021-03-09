const Migrations = artifacts.require("./Migrations");
const Bank = artifacts.require('./Bank.sol');
const ShareHolders = artifacts.require('./ShareHolders.sol');

module.exports = function (deployer) {
  deployer.deploy(Migrations);
  deployer.deploy(Bank);
  deployer.deploy(ShareHolders, 200);
};
