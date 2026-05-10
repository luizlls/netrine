use super::ir::*;
use crate::error::Result;
use crate::interner::Interner;

#[derive(Debug)]
struct Context<'ctx> {
    mir: Module,
    interner: &'ctx Interner,
}

impl<'ctx> Context<'ctx> {
    const fn new(mir: Module, interner: &'ctx Interner) -> Context<'ctx> {
        Context { mir, interner }
    }

    fn finalize(self) -> Module {
        self.mir
    }
}

fn type_check(ctx: &mut Context) {
    todo!()
}

pub fn check(mir: Module, interner: &Interner) -> Result<Module> {
    let mut context = Context::new(mir, interner);
    type_check(&mut context);

    Ok(context.finalize())
}
