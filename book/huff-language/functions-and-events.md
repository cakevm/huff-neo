# Define functions and events

While defining an interface is not a necessary step, `functions` and `events`
can be defined in Huff contracts for two purposes: To be used as arguments
for the `__FUNC_SIG` and `__EVENT_HASH` builtins, and to generate a Solidity
Interface / Contract ABI.

Functions can be of type `view`, `pure`, `payable` or `nonpayable`, and
function interfaces should only be defined for externally facing functions.

Events can contain `indexed` and non-indexed values.

Parameters of functions, events and errors can be tuples, the ABI form of a Solidity struct, written as their component types in parentheses.
Component names are optional, and a tuple can be nested or used as an array. The selector uses the canonical
type, so `swap((address maker, uint256 amount) order)` has the selector of `swap((address,uint256))`, the same
as a Solidity function taking a struct with those fields. Each tuple becomes a struct in the interface `-g`
generates.

## Example
Define a function:
```javascript
#define function testFunction(uint256, bytes32) view returns (bytes memory)
```

Define a function that takes a struct and returns an array of them:
```javascript
#define function fill((address maker, uint256 amount) order) nonpayable returns ((address,uint256)[])
```

Define an event:
```javascript
#define event TestEvent(address indexed, uint256)
```

Define an error that takes a struct:
```javascript
#define error InvalidOrder((address maker, uint256 amount) order)
```