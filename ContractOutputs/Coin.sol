// SPDX-License-Identifier: GPL-3.0
pragma solidity >=0.4.22 <0.8.0;

contract Coin {
	address public minter;
	mapping (address => uint) public balances;
	function Coin() public {
		minter = msg.sender;
	}
	function mint(address receiver, uint amount) public {
		if (msg.sender != minter) {
			return;
		}
		balances = balances + amount;
	}
}
