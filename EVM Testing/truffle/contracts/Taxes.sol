// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract Taxes {
	constructor (uint _taxRate) {
		owner = msg.sender; 
		contractBalance = 0; 
		taxRate = _taxRate; 
	}
	address payable public owner; 
	uint public contractBalance; 
	mapping (address => uint) balance; 
	uint public taxRate; 
	function customerWithdrawal(uint amount) public {
		require(balance[msg.sender] >= amount); 
		balance[msg.sender] -= amount; 
		payable(msg.sender).transfer(amount); 
	}
	function customerDeposit() public payable {
		balance[msg.sender] += msg.value; 
	}
	function payTaxes(address payable _to, uint amount) public payable {
		amount = balance[_to] * taxRate; 
		require(balance[msg.sender] >= amount); 
		balance[msg.sender] -= amount; 
		balance[_to] += amount; 
	}
}
