// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract RichestGame {
	constructor (address payable _richest, uint _contractBalance, uint _start, uint _end) {
		richest = _richest; 
		contractBalance = _contractBalance; 
		start = _start; 
		end = _end; 
	}
	address payable public richest; 
	uint public contractBalance; 
	uint public start; 
	uint public end; 
	function attemptToBecomeRichest() public payable {
		contractBalance += msg.value; 
		require(block.timestamp >= start && block.timestamp <= end); 
		if (msg.value > contractBalance) {
			richest = msg.sender; 
		}
	}
	function withdraw() public {
		require(block.timestamp > end); 
		require(richest == msg.sender); 
		payable(msg.sender).transfer(contractBalance); 
		contractBalance = 0; 
	}
}
