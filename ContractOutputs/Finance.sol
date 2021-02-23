// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract Finance {
	constructor () {
		owner = msg.sender; 
	}
	mapping (address => uint) balance; 
	address payable public owner; 
	function withdraw(uint amount) public {
		require(balance[msg.sender] >= amount); 
		balance[msg.sender] -= amount; 
		payable(msg.sender).transfer(amount); 
	}
	function deposit() public payable {
		balance[msg.sender] += msg.value; 
	}
	function transfer(address payable _to, uint amount) public payable {
		require(balance[msg.sender] >= amount); 
		balance[msg.sender] -= amount; 
		balance[_to] += amount; 
	}
}
