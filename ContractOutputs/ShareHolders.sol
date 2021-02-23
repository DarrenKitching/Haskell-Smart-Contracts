// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract ShareHolders {
	constructor () {
		owner = msg.sender; 
	}
	address payable public owner; 
	mapping (address => uint) shares; 
	mapping (address => uint) balance; 
	function withdraw() public {
		uint userBalance = balance[msg.sender];
		balance[msg.sender] -= userBalance; 
		payable(msg.sender).transfer(userBalance); 
	}
}
