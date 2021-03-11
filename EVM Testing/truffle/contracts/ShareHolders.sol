// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract ShareHolders {
	constructor (uint _totalShares) {
		contractBalance = 0; 
		owner = msg.sender; 
		totalShares = _totalShares; 
	}
	uint public contractBalance; 
	address payable public owner; 
	uint public totalShares; 
	mapping (address => uint) shares; 
	mapping (address => uint) balance; 
	function withdraw() public {
		uint amount = balance[msg.sender];
		balance[msg.sender] -= amount; 
		payable(msg.sender).transfer(amount); 
	}
	function payDividends(address payable _to, uint amount) public payable {
		require(owner == msg.sender); 
		amount /= shares[_to] / totalShares; 
		require(contractBalance >= amount); 
		contractBalance -= amount; 
		balance[_to] += amount; 
	}
	function depositToContract() public payable {
		require(owner == msg.sender); 
		contractBalance += msg.value; 
	}
	function setShares(address payable shareHolder, uint sharesOwned) public {
		require(owner == msg.sender); 
		shares[shareHolder] = sharesOwned; 
	}
	function getBalance() public view returns (uint){
		return balance[msg.sender];
	}
}
