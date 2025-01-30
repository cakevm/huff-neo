# Define functions and events

While defining an interface is not a necessary step, `functions` and `events`
can be defined in Huff contracts for two purposes: To be used as arguments
for the `__FUNC_SIG` and `__EVENT_HASH` builtins, and to generate a Solidity
Interface / Contract ABI.

Functions can be of type `view`, `pure`, `payable` or `nonpayable`, and
function interfaces should only be defined for externally facing functions.

Events can contain `indexed` and non-indexed values.

## Example
Define a function:
```javascript
#define function testFunction(uint256, bytes32) view returns (bytes memory)
```

Define an event:
```javascript
#define event TestEvent(address indexed, uint256)
```