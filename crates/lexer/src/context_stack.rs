use huff_neo_utils::error::LexicalErrorKind;
use huff_neo_utils::lexer_context::Context;

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
    pub fn pop(&mut self) -> Result<(), LexicalErrorKind> {
        self.stack.pop();
        if self.stack.is_empty() {
            return Err(LexicalErrorKind::StackUnderflow);
        }
        Ok(())
    }

    /// Returns the last context on the stack
    pub fn top(&self) -> &Context {
        self.stack.last().unwrap()
    }
}
