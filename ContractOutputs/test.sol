// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.7.4;
contract Test {
	constructor (int _test) {
		contractBalance = 0; 
		test = _test; 
	}
	uint public contractBalance; 
	int public test; 
	function LoopTest() public {
		for (int i = 0;i < test; i++) {
			payable(msg.sender).transfer(contractBalance); 
			contractBalance = 0; 
		}
	}
}
