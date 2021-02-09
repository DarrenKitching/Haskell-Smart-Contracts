// SPDX-License-Identifier: GPL-3.0
contract Ballot {
	struct Voter {uint weight;bool voted;address delegate;uint vote;}
	struct Proposal {bytes32 name;uint voteCount;}
	mapping (address => Voter) public voters; 
	Proposal[] public proposals; 
}
