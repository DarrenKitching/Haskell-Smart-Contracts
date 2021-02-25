// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract ShareHolders {
	constructor () {
		contractBalance = 0; 
		owner = msg.sender; 
	}
	uint public contractBalance; 
	address payable public owner; 
	mapping (address => uint) shares; 
	mapping (address => uint) balance; 
	function withdraw() public {
		uint userBalance = balance[msg.sender];
		balance[msg.sender] -= userBalance; 
		payable(msg.sender).transfer(userBalance); 
	}
	function payDividends(address payable _to, uint amount) public payable {
		require(owner == msg.sender); 
		amount /= shares[_to] / 100; 
		require(contractBalance >= amount); 
		contractBalance -= amount; 
		balance[_to] += amount; 
	}
	function depositToContract() public payable {
		require(owner == msg.sender); 
		contractBalance += msg.value; 
	}
}
