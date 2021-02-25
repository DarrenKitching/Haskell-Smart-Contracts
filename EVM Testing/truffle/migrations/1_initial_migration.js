const Migrations = artifacts.require("./Migrations");
const Bank = artifacts.require('./Bank.sol');

module.exports = function (deployer) {
  deployer.deploy(Migrations);
  deployer.deploy(Bank);
};
