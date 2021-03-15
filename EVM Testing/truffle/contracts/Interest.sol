// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract Interest {
	constructor (uint _interestRate, uint _DIRT) {
		owner = msg.sender; 
		contractBalance = 0; 
		interestRate = _interestRate; 
		DIRT = _DIRT; 
	}
	address payable public owner; 
	uint public contractBalance; 
	mapping (address => uint) balance; 
	uint public interestRate; 
	uint public DIRT; 
	function customerWithdrawal(uint amount) public {
		require(balance[msg.sender] >= amount); 
		balance[msg.sender] -= amount; 
		payable(msg.sender).transfer(amount); 
	}
	function customerDeposit() public payable {
		balance[msg.sender] += msg.value; 
	}
	function payInterest(address payable _to, uint amount) public payable {
		amount = balance[_to] * interestRate / 100; 
		require(contractBalance >= amount); 
		contractBalance -= amount; 
		balance[_to] += amount; 
	}
	function payInterestLessDIRT(address payable _to, uint amount) public payable {
		amount = balance[_to] * interestRate / 100; 
		amount = amount * (100 - DIRT) / 100; 
		require(contractBalance >= amount); 
		contractBalance -= amount; 
		balance[_to] += amount; 
	}
	function giveOwnership(address payable newOwner) public {
		require(owner == msg.sender); 
		owner = newOwner; 
	}
	function getBalance() public view returns (uint){
		return balance[msg.sender];
	}
	function depositToContract() public payable {
		require(owner == msg.sender); 
		contractBalance += msg.value; 
	}
}
