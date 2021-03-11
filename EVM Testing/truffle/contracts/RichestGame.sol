// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract RichestGame {
	constructor (address payable _richest, uint _start, uint _end, uint _highestAmount) {
		richest = _richest; 
		contractBalance = 0; 
		start = _start; 
		end = _end; 
		highestAmount = _highestAmount; 
	}
	address payable public richest; 
	uint public contractBalance; 
	uint public start; 
	uint public end; 
	uint public highestAmount; 
	function attemptToBecomeRichest() payable public {
		require(block.timestamp >= start && block.timestamp <= end); 
		contractBalance += msg.value; 
		if (msg.value > highestAmount) {
			richest = msg.sender; 
			highestAmount = msg.value; 
		}
	}
	function withdraw() public {
		require(block.timestamp > end); 
		require(richest == msg.sender); 
		payable(msg.sender).transfer(contractBalance); 
		contractBalance = 0; 
	}
}
