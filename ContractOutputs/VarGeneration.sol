// SPDX-License-Identifier: GPL-3.0
pragma solidity >=0.4.22 <0.8.0;

contract VarExample {
	struct Voter {
		int voterID;
		string voterName;
	}
	struct PollingStation {
		int pollingStationID;
		string voterAddress;
	}
	string value;
	function getvalue() public view returns(string memory) {
		return value;
	}
	function setvalue(string memory _value) public {
		value = _value;
	}
	int myInteger;
	function getmyInteger() public view returns(int) {
		return myInteger;
	}
	function setmyInteger(int _myInteger) public {
		myInteger = _myInteger;
	}
	uint myUnsignedInteger;
	function getmyUnsignedInteger() public view returns(uint) {
		return myUnsignedInteger;
	}
	function setmyUnsignedInteger(uint _myUnsignedInteger) public {
		myUnsignedInteger = _myUnsignedInteger;
	}
	bool myBool;
	function getmyBool() public view returns(bool) {
		return myBool;
	}
	function setmyBool(bool _myBool) public {
		myBool = _myBool;
	}
	bytes myBytes;
	function getmyBytes() public view returns(bytes memory) {
		return myBytes;
	}
	function setmyBytes(bytes memory _myBytes) public {
		myBytes = _myBytes;
	}
}