<h1>Overview</h1>
<p>This project involved building an embedded domain specific language in Haskell that could output Smart Contracts runnable on the Ethereum Virtual Machine.</p>
</br>

<h1>Write your own Contracts</h1>
<p>In the FinancialExamples.hs file you will find seven example contracts that have been written using the DSL. You can begin writing new contracts here. You should define your contract variables and contract functions and then create an instance of the Contract data-type and assign it to some variable. <br><br>
In order to ensure that the new contract you have written is outputted when the program is next run, you need to go to the Main.hs file and inside of the main function do block pass the variable representing the contract along with a string representing the output destination to the outputContract function.</p>
</br>

<h1>Run Instructions</h1>
<h2>Run DSL</h2>
<p>To run the program you first need to install stack. Details can be found here: https://docs.haskellstack.org/en/stable/install_and_upgrade/<br>You then need to clone the repository and inside of the project folder run:

```sh
stack build
```
followed by:
```sh
stack exec HaskellContracts
```
The sample DSL contracts are currently being outputted to the ContractOutputs folder as Solidity files.
</p>
<h2>Run Unit Tests</h2>
<p>Unit tests have been written for the seven DSL example contracts implemented. To run these tests you first need to install Node.js and npm, details can be found here: https://docs.npmjs.com/downloading-and-installing-node-js-and-npm. <br>This will allow you to install the Solidity compiler using:
  
```sh
npm install -g solc
```
You will then need to install testrpc using:

```sh
npm install -g ethereumjs-testrpc
```
Followed by installing truffle using:
```sh
npm install -g truffle
```
To run the tests, you then need to open two seperate terminal windows and in one of those terminals run testrpc to simulate the actors in your blockchain using:
```sh
testrpc
``` 
in the second terminal you should then navigate to the "EVM Testing/truffle" folder and run:
```sh
sudo truffle migrate
``` 
followed by: 
```sh
sudo truffle test
```
</p>
