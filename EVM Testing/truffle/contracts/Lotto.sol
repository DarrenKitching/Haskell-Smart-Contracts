// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract Lotto {
	constructor (address payable _winner, uint _numberOfPlayers, uint playersSize, uint _ticketPrice) {
		winner = _winner; 
		numberOfPlayers = _numberOfPlayers; 
		players = new address payable[](playersSize); 
		ticketPrice = _ticketPrice; 
		contractBalance = 0; 
		owner = msg.sender; 
	}
	address payable public winner; 
	uint public numberOfPlayers; 
	address payable[] public players; 
	uint public ticketPrice; 
	uint public contractBalance; 
	address payable public owner; 
	function buyTicket(uint amount) payable public {
		if (msg.value == ticketPrice) {
			require(msg.value == amount); 
			contractBalance += msg.value; 
			players[numberOfPlayers] = msg.sender; 
			numberOfPlayers = numberOfPlayers + 1; 
		}
	}
	function pickWinner() public {
		require(numberOfPlayers != 0); 
		require(owner == msg.sender); 
		winner = players[uint(uint256(keccak256(abi.encodePacked(block.timestamp, block.difficulty)))%numberOfPlayers)]; 
	}
	function withdraw() public {
		require(winner == msg.sender); 
		payable(msg.sender).transfer(contractBalance); 
		contractBalance = 0; 
	}
	function checkWinner() public view returns (address payable){
		return winner;
	}
}
