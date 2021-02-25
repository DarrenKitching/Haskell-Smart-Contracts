// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract Interest {
	constructor (uint _interestRate) {
		owner = msg.sender; 
		contractBalance = 0; 
		interestRate = _interestRate; 
	}
	address payable public owner; 
	uint public contractBalance; 
	mapping (address => uint) balance; 
	uint public interestRate; 
	function customerWithdrawal(uint amount) public {
		require(balance[msg.sender] >= amount); 
		balance[msg.sender] -= amount; 
		payable(msg.sender).transfer(amount); 
	}
	function customerDeposit() public payable {
		balance[msg.sender] += msg.value; 
	}
	function payInterest(address payable _to, uint amount) public payable {
		amount = balance[_to] * interestRate; 
		require(contractBalance >= amount); 
		contractBalance -= amount; 
		balance[_to] += amount; 
	}
	function giveOwnership(address payable newOwner) public {
		require(owner == msg.sender); 
		owner = newOwner; 
	}
}
