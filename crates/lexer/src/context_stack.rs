use huff_neo_utils::error::LexicalErrorKind;
use huff_neo_utils::lexer_context::Context;

// The context stack is used to keep track of the current context.
//
// Complete context hierarchy showing all possible parent-child relationships:
//
// Context::Global
// ├─ Context::Constant
// ├─ Context::Abi
// │  └─ Context::AbiArgs
// ├─ Context::MacroDefinition
// │  └─ Context::MacroBody
// │     ├─ Context::BuiltinFunction
// │     ├─ Context::MacroArgs
// │     └─ Context::ForLoopBody
// │        ├─ Context::BuiltinFunction
// │        ├─ Context::MacroArgs
// │        └─ Context::ForLoopBody (nested)
// └─ Context::CodeTableBody

/// The context stack is used to keep track of the current context
pub struct ContextStack {
    /// The stack of contexts
    stack: Vec<Context>,
}

impl ContextStack {
    /// Creates a new context stack with the global context
    pub fn new() -> Self {
        ContextStack { stack: vec![Context::Global] }
    }

    /// Replaces the last context on the stack with a new context
    pub fn replace(&mut self, context: Context) {
        let last = self.stack.last_mut().unwrap();
        *last = context;
    }

    /// Pushes a new context onto the stack
    pub fn push(&mut self, context: Context) {
        self.stack.push(context);
    }

    /// Pops the last context off the stack
    pub fn pop(&mut self, count: u8) -> Result<(), LexicalErrorKind> {
        for _ in 0..count {
            self.stack.pop();
            if self.stack.is_empty() {
                return Err(LexicalErrorKind::StackUnderflow);
            }
        }

        Ok(())
    }

    /// Returns the last context on the stack
    pub fn top(&self) -> &Context {
        self.stack.last().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use huff_neo_utils::lexer_context::Context;

    #[test]
    fn test_context_stack_new() {
        let stack = ContextStack::new();
        assert_eq!(stack.top(), &Context::Global);
    }

    #[test]
    fn test_context_stack_replace() {
        let mut stack = ContextStack::new();
        stack.push(Context::MacroDefinition);
        assert_eq!(stack.top(), &Context::MacroDefinition);

        stack.replace(Context::MacroBody);
        assert_eq!(stack.top(), &Context::MacroBody);
    }

    #[test]
    fn test_context_stack_push_pop() {
        let mut stack = ContextStack::new();
        stack.push(Context::MacroDefinition);
        assert_eq!(stack.top(), &Context::MacroDefinition);

        stack.pop(1).unwrap();
        assert_eq!(stack.top(), &Context::Global);
    }

    #[test]
    fn test_context_stack_underflow() {
        let mut stack = ContextStack::new();
        stack.pop(1).unwrap_err();
    }

    #[test]
    fn test_context_stack_push_pop_multiple() {
        let mut stack = ContextStack::new();
        stack.push(Context::Abi);
        stack.push(Context::AbiArgs);
        assert_eq!(stack.top(), &Context::AbiArgs);

        stack.pop(1).unwrap();
        assert_eq!(stack.top(), &Context::Abi);

        stack.pop(1).unwrap();
        assert_eq!(stack.top(), &Context::Global);
    }
}
