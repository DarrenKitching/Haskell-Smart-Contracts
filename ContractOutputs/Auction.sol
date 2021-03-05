// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract Auction {
	constructor (address payable _winner, uint _contractBalance, uint _highestBid, uint _start, uint _end) {
		winner = _winner; 
		owner = msg.sender; 
		contractBalance = _contractBalance; 
		highestBid = _highestBid; 
		start = _start; 
		end = _end; 
	}
	address payable public winner; 
	address payable public owner; 
	uint public contractBalance; 
	uint public highestBid; 
	uint public start; 
	uint public end; 
	function withdraw() public {
		require(block.timestamp > end); 
		require(owner == msg.sender); 
		payable(msg.sender).transfer(contractBalance); 
		contractBalance = 0; 
	}
	function bid() public {
		require(block.timestamp >= start && block.timestamp <= end); 
		if (newBid > highestBid) {
			winner = msg.sender; 
			highestBid = newBid; 
		}
	}
	function payIfWinner() public payable {
		require(block.timestamp > end); 
		require(winner == msg.sender); 
		if (highestBid == msg.value) {
			contractBalance += msg.value; 
		}
	}
	function checkWinner() public view returns (address payable){
		return winner;
	}
}
