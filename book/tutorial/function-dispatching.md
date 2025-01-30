# Function Dispatching

Function dispatching is something that is fundamental to any Huff contract. Unlike Solidity and Vyper; Huff does not abstract function dispatching. In this section we will go over how dispatching is performed in the other languages, and how you may go about it in Huff.

## What is the problem?

In the evm, contracts are interacted with by sending messages to them. The ABI standard exists as a canonical way to encode these messages, handling how inputs to functions should be encoded. These strict rules are what allow contracts to understand each other. The ABI standard also tells the contract which function the message intends to interact with. This is done by encoding a 4 byte selector at the beginning of the message. This 4 byte selector is the first 4 bytes of the `keccak` of the function's signature string. For example, the keccak256 hash of the string "myFunction(address,uint256)" is `0x451c00ddd225deee9948ba5eca26042a5ea1cc0980e5a5fb0a057f90567af5e0`. So the first 4 bytes `0x451c00dd` is what Solidity uses as the function signature. If you have written interfaces in solidity you may not realize that you are just providing your current contract the ability to generate the 4 byte selectors of a foreign contract.

The rest of this section will detail two types of dispatching, linear dispatching and binary search dispatching.

## Linear Dispatching

From reading the above, or from reading a Huff contract before you may have developed some intuition about how the simplest way to perform dispatching is - A linear lookup. This method will extract the function selector from the calldata message, then brute force compare it to every other function in the contract.

The example below shows what a linear dispatcher may look like for a standard ERC20 token:

```javascript

// Interface
#define function allowance(address,address) view returns (uint256)
#define function approve(address,uint256) nonpayable returns ()
#define function balanceOf(address) view returns (uint256)
#define function DOMAIN_SEPARATOR() view returns (bytes32)
#define function nonces(address) view returns (uint256)
#define function permit(address,address,uint256,uint256,uint8,bytes32,bytes32) nonpayable returns ()
#define function totalSupply() view returns (uint256)
#define function transfer(address,uint256) nonpayable returns ()
#define function transferFrom(address,address,uint256) nonpayable returns ()
#define function decimals() nonpayable returns (uint256)
#define function name() nonpayable returns (string)
#define function symbol() nonpayable returns (string)

// Function Dispatching
#define macro MAIN() = takes (1) returns (1) {
    // Identify which function is being called.
    0x00 calldataload 0xE0 shr          // [func_sig]

    dup1 __FUNC_SIG(permit)             eq permitJump           jumpi
    dup1 __FUNC_SIG(nonces)             eq noncesJump           jumpi

    dup1 __FUNC_SIG(name)               eq nameJump             jumpi
    dup1 __FUNC_SIG(symbol)             eq symbolJump           jumpi
    dup1 __FUNC_SIG(decimals)           eq decimalsJump         jumpi
    dup1 __FUNC_SIG(DOMAIN_SEPARATOR)   eq domainSeparatorJump  jumpi

    dup1 __FUNC_SIG(totalSupply)        eq totalSupplyJump      jumpi
    dup1 __FUNC_SIG(balanceOf)          eq balanceOfJump        jumpi
    dup1 __FUNC_SIG(allowance)          eq allowanceJump        jumpi

    dup1 __FUNC_SIG(transfer)           eq transferJump         jumpi
    dup1 __FUNC_SIG(transferFrom)       eq transferFromJump     jumpi
    dup1 __FUNC_SIG(approve)            eq approveJump          jumpi

    // Revert if no match is found.
    0x00 dup1 revert

    allowanceJump:
        ALLOWANCE()
    approveJump:
        APPROVE()
    balanceOfJump:
        BALANCE_OF()
    decimalsJump:
        DECIMALS()
    domainSeparatorJump:
        DOMAIN_SEPARATOR()
    nameJump:
        NAME()
    noncesJump:
        NONCES()
    permitJump:
        PERMIT()
    symbolJump:
        SYMBOL()
    totalSupplyJump:
        TOTAL_SUPPLY()
    transferFromJump:
        TRANSFER_FROM()
    transferJump:
        TRANSFER()
}
```

There is one extremely important piece of code you will use in almost all of your Huff contracts:

```plaintext
0x00 calldataload 0xE0 shr
```

This loads the four byte function selector onto the stack. `0x00 calldataload` will load 32 bytes starting from position 0 onto the stack (if the calldata is less than 32 bytes then it will be right padded with zeros). `0xE0 shr` right shifts the calldata by 224 bits, leaving 32 bits or 4 bytes remaining on the stack.

Despite this seeming like a rather naive approach, for most contracts it is often the most effective. As this is one large `if` `else if` chain, you can optimize by placing "hot functions" towards the top of your chain. Functions towards the front will cost less gas to invoke, but be aware as your function approaches the end of the chain it can really get expensive!

This method seems naive, however this is exactly how Vyper and Solidity\* implement linear dispatching. If you want it to be cheaper to call, just move it higher up in the contract!

\* Solidity implements this method when there are fewer than 5 functions, or the number of optimizer runs is small. Otherwise, it will use binary search dispatching.

You may be wondering what the series of jump labels followed by implementation macros mean. A good mental model is that the entire of your contract exists within your MAIN macro. If a macro is not referred to within main or nested within a macro that is invoked within MAIN then it will not be included in your contract. In reality the above contract when compiled looks like this:

```plaintext
0x[dispatcher]<allowanceJumpLabel>[ALLOWANCE MACRO]<approveJumpLabel>[APPROVE MACRO]<balanceOfJump>[BALANCE_OF MACRO] ... <transferFromLabel>[TRANSFER_FROM MACRO]<transferJump>[TRANSFER MACRO]
```

In Huff all of your macros are inlined into one long bytecode string (this is also true for Vyper and Solidity!). Due to this property it is salient that you terminate ALL of your top-level macros with some form of escape opcode (`return`, `stop`, `revert`). If you don't do this your macro _WILL_ continue to execute which ever macro is inlined next until a return condition is found.

For example:

```javascript
#define function returnOne() public returns(uint256)
#define function returnTwo() public returns(uint256)

/// @notice returns the value one
#define macro RETURN_ONE() = {
    0x01 0x00 mstore
    0x20 0x00 return
}

/// @notice places the value 2 onto the stack
#define macro RETURN_TWO() = {
    0x02
}

#define macro MAIN() = {
    0x00 calldataload 0xE0 shr

    // Dispatcher
    dup1 __FUNC_SIG(returnOne) eq returnOneJump jumpi
    dup1 __FUNC_SIG(returnTwo) eq returnTwoJump jumpi

    // Macros
    returnTwo:
        RETURN_TWO()
    returnOne:
        RETURN_ONE()
}
```

In the above macro, regardless if what message is sent to it, it will ALWAYS return the value 1. As the macro `RETURN_TWO` does not terminate, it will roll over into the execution of `RETURN_ONE`, terminating with returning the value one. As there is no guard to protect against non valid function signatures any message, even if it matches nothing in the dispatcher will return the value 1.

One way we can handle no valid function selector being found is by inserting `0x00 0x00 revert` in-between our dispatcher and our macro jump labels as so:

```javascript
#define macro MAIN() = {
    0x00 calldataload 0xE0 shr

    dup1 __FUNC_SIG(returnOne) eq returnOneJump jumpi
    dup1 __FUNC_SIG(returnTwo) eq returnTwoJump jumpi

    0x00 0x00 revert

    returnTwo:
        RETURN_TWO()
    returnOne:
        RETURN_ONE()
}
```

Please keep this behavior in mind when writing Huff contracts, especially when dealing with administration functions, running into a `SET_OWNER()` macro could allow anyone to take control of your contract. There is a more advanced way of jumping over the macro calling logic that is used with Huff "inheritance" pattern that we will discuss in another section.

## Binary Search Dispatching

Another method of function dispatching is by doing a binary search to find the correct selector. This is great for contracts with lots and lots of functions as it makes the dispatching cost more predictable / steady (No more checking every single branch of the `if else` chain). In this method, we order our function selectors by their keccak, then pivot about a number of jump points until we reach the desired function. The number of jump points you include is up to you. The more jumps you add the more consistent your jump price will be, however, be mindful of the gas cost for comparisons. Generally, each split will add 16-18 bytes of additional code (remember there is a jump out of each pivot point).

To implement this approach you will need to manually calculate the function selectors and order them by hand. But do not worry, this can be done easily with a script.

Here is an example implementation of a binary search dispatch.

```javascript

// Define Interface
#define function allowance(address,address) view returns (uint256)
#define function approve(address,uint256) nonpayable returns ()
#define function balanceOf(address) view returns (uint256)
#define function DOMAIN_SEPARATOR() view returns (bytes32)
#define function nonces(address) view returns (uint256)
#define function permit(address,address,uint256,uint256,uint8,bytes32,bytes32) nonpayable returns ()
#define function totalSupply() view returns (uint256)
#define function transfer(address,uint256) nonpayable returns ()
#define function transferFrom(address,address,uint256) nonpayable returns ()
#define function decimals() nonpayable returns (uint256)
#define function name() nonpayable returns (string)
#define function symbol() nonpayable returns (string)

// Function Dispatching
#define macro MAIN() = takes (1) returns (1) {
    // Identify which function is being called.
    // [func sig]
    0x00 calldataload 0xE0 shr

    // The function selector of the pivot (number of selectors / 2)
    dup1 __FUNC_SIG(balanceOf) lt pivot0 jumpi

        // pivot 2
        dup1 __FUNC_SIG(totalSupply) lt pivot00 jumpi

            // 1
            dup1 __FUNC_SIG(name)               eq nameJump             jumpi

            // 2
            dup1 __FUNC_SIG(approve)            eq approveJump          jumpi

            // 3
            dup1 __FUNC_SIG(totalSupply)        eq totalSupplyJump      jumpi

            not_found jump

        pivot00:

            // 4
            dup1 __FUNC_SIG(transferFrom)       eq transferFromJump     jumpi

            // 5
            dup1 __FUNC_SIG(decimals)           eq decimalsJump         jumpi

            // 6
            dup1 __FUNC_SIG(DOMAIN_SEPARATOR)   eq domainSeparatorJump  jumpi

            not_found jump

    pivot0:

        dup1 __FUNC_SIG(symbol) lt pivot11 jumpi


            // 7
            dup1 __FUNC_SIG(balanceOf)          eq balanceOfJump        jumpi

            // 8
            dup1 __FUNC_SIG(nonces)             eq noncesJump           jumpi

            // 9
            dup1 __FUNC_SIG(symbol)             eq symbolJump           jumpi

            not_found jump

        pivot11:

            // 10
            dup1 __FUNC_SIG(transfer)           eq transferJump         jumpi

            // 11
            dup1  __FUNC_SIG(permit)             eq permitJump           jumpi

            // 12
            dup1 __FUNC_SIG(allowance)          eq allowanceJump        jumpi

    not_found:

    // Revert if no match is found.
    0x00 dup1 revert

    allowanceJump:
        ALLOWANCE()
    approveJump:
        APPROVE()
    balanceOfJump:
        BALANCE_OF()
    decimalsJump:
        DECIMALS()
    domainSeparatorJump:
        DOMAIN_SEPARATOR()
    nameJump:
        NAME()
    noncesJump:
        NONCES()
    permitJump:
        PERMIT()
    symbolJump:
        SYMBOL()
    totalSupplyJump:
        TOTAL_SUPPLY()
    transferFromJump:
        TRANSFER_FROM()
    transferJump:
        TRANSFER()
}
```

## Fallback functions

In solidity there are two special functions, fallback and receive. Both are relatively straightforward to implement in Huff.

To implement fallback, just place a macro at the end of your dispatch logic, take for example the following fallback function that will always return the value 1:

```javascript
#define macro FALLBACK() = {
    0x01 0x00 mstore
    0x20 0x00 return
}
```

Implementing it as a fallback is as simple as throwing it after you have exhausted your switch cases.

```javascript
#define macro MAIN() = takes (1) returns (1) {
    // Identify which function is being called.
    // [func sig]
    0x00 calldataload 0xE0 shr

    dup1 __FUNC_SIG(permit)             eq permitJump           jumpi

    ...

    dup1 __FUNC_SIG(approve)            eq approveJump          jumpi

    FALLBACK()

   permitJump:
        PERMIT()

    ...

    approveJump:
        APPROVE()
}
```

If you want to implement both fallback and receive macros you can do the following:

```javascript
#define macro MAIN() = takes (1) returns (1) {
    // Identify which function is being called.
    // [func sig]
    0x00 calldataload 0xE0 shr

    dup1 __FUNC_SIG(permit)             eq permitJump           jumpi

    ...

    dup1 __FUNC_SIG(approve)            eq approveJump          jumpi

    # Jump into the receive function if msg.value is not zero
    callvalue receive jumpi

    FALLBACK()

    receive:
        RECEIVE()

    permitJump:
        PERMIT()

    ...

    approveJump:
        APPROVE()
}
```
