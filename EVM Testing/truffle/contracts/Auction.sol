// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract Auction {
	constructor (address payable _winner, uint _highestBid, bool _hasPaid, uint _start, uint _end) {
		winner = _winner; 
		owner = msg.sender; 
		contractBalance = 0; 
		highestBid = _highestBid; 
		hasPaid = _hasPaid; 
		start = _start; 
		end = _end; 
	}
	address payable public winner; 
	address payable public owner; 
	uint public contractBalance; 
	uint public highestBid; 
	bool public hasPaid; 
	uint public start; 
	uint public end; 
	function withdraw() public {
		require(block.timestamp > end); 
		require(owner == msg.sender); 
		require(hasPaid == true); 
		payable(msg.sender).transfer(contractBalance); 
		contractBalance = 0; 
	}
	function bid(uint newBid) public {
		require(block.timestamp >= start && block.timestamp <= end); 
		if (newBid > highestBid) {
			winner = msg.sender; 
			highestBid = newBid; 
		}
	}
	function payIfWinner() payable public {
		require(block.timestamp > end); 
		require(winner == msg.sender); 
		if (highestBid == msg.value) {
			contractBalance += msg.value; 
			hasPaid = true; 
		}
	}
	function checkWinner() public view returns (address payable){
		return winner;
	}
}
