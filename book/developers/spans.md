# Understanding Spans

Spans are a fundamental concept in the Huff Neo compiler that track the source location of tokens, AST nodes, and bytecode. They enable accurate error reporting and source mapping for debugging.

## What is a Span?

A `Span` represents a range of characters in the source code:

```rust,ignore
pub struct Span {
    pub start: usize,  // Byte offset where the span starts
    pub end: usize,    // Byte offset where the span ends (exclusive)
    pub file: Option<Arc<FileSource>>,  // Reference to the source file
}
```

## Exclusive End Convention

Huff Neo uses **exclusive end positions**, following Rust's `Range` convention:

- `start` points to the first character of the span
- `end` points to the position **after** the last character
- This matches Rust's range syntax: `source[start..end]`

### Example

Given the source code:
```huff
#define macro MAIN() = takes(0) returns (0) {
    0x42
    add
}
```

The span for the token `0x42` would be:
- Text: `"0x42"`
- Start: 50 (position of '0')
- End: 54 (position after '2')
- Length: 4 characters
- Extract with: `source[50..54]` → `"0x42"`

## Working with Spans

### Creating Spans

The lexer creates spans as it tokenizes:

```rust,ignore
// In the lexer, when we recognize a token:
let start = self.position;  // Current position
let token_text = self.eat_while(|c| c.is_alphanumeric());
let end = self.position + token_text.len();  // Exclusive end

Token {
    kind: TokenKind::Ident(token_text),
    span: Span::new(start..end, file)
}
```

### Using Spans for Error Reporting

Spans enable precise error messages:

```rust,ignore
// When reporting an error:
return Err(CodegenError {
    kind: CodegenErrorKind::UnmatchedJumpLabel,
    span: label.span,  // Points exactly to the problematic label
    token: Some(label.clone()),
});
```

This produces output like:
```text
Error: Unmatched jump label
   --> example.huff:5:8
    |
  5 |     unknown_label jumpi
    |     ^^^^^^^^^^^^^
```

### Span Preservation During Compilation

Spans flow through the compilation pipeline:

1. **Lexer** → Creates initial spans for tokens
2. **Parser** → Combines token spans into AST node spans
3. **Codegen** → Preserves spans when generating bytecode
4. **Source Maps** → Maps bytecode positions back to source spans

## Special Cases

### Macro Expansion

When expanding macros, we preserve the span of the actual operation, not the invocation:

```huff
#define macro ADD() = takes(2) returns (1) {
    add  // Span points here
}

#define macro MAIN() = takes(0) returns (0) {
    0x01
    0x02
    ADD()  // NOT here - we want the span of 'add', not 'ADD()'
}
```

The source map for the `add` opcode points to line 2, not line 8.

### Constants and Arguments

For constants and macro arguments, the span includes the brackets:

```huff
#define constant VALUE = 0x42

#define macro MAIN() = takes(0) returns (0) {
    [VALUE]  // Span covers all of "[VALUE]" including brackets
}
```

### Jump Labels

Jump labels have two different spans:

1. **Label definition**: `label:` - Span covers "label:"
2. **Label reference**: `label jumpi` - Span covers just "label"

## Common Pitfalls

### Off-by-One Errors

Remember that `end` is exclusive:

```rust,ignore
// WRONG - treats end as inclusive
let text = &source[span.start..=span.end];  

// CORRECT - end is exclusive
let text = &source[span.start..span.end];
```

### Multi-byte Characters

Spans use byte offsets, not character counts. Be careful with UTF-8:

```rust,ignore
// The lexer handles this correctly by using byte positions
let end_position = self.position + char.len_utf8();
```

### Empty Spans

Empty spans (where `start == end`) are valid and used for:
- EOF tokens
- Inserted/synthetic tokens
- Zero-width positions