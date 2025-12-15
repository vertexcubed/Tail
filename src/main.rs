use crate::vm::vm::TailVirtualMachine;
use std::time::Instant;
use crate::ast::visit::AstVisitor;
use crate::ast::Stmt;
use crate::compile::visit::Compiler;
use crate::ty::visit::TypeVisitor;

mod vm;
#[allow(dead_code)]
mod ast;
#[allow(dead_code)]
mod ty;

mod compile;

/// Functions for making ASTs by hand. Solely for debugging
#[allow(dead_code)]
mod debug;

// TODO: come up with a better way of doing this
macro_rules! impl_id {
    ($typ:ty) => {
        impl std::ops::Add for $typ {
            type Output = $typ;
            fn add(self, rhs: Self) -> Self::Output {
                Self(self.0 + rhs.0)
            }
        }
        impl std::ops::Sub for $typ {
            type Output = $typ;
            fn sub(self, rhs: Self) -> Self::Output {
                Self(self.0 - rhs.0)
            }
        }
        impl std::ops::AddAssign<$typ> for $typ {
            fn add_assign(&mut self, rhs: Self) {
                self.0 += rhs.0;
            }
        }
        impl std::ops::SubAssign<$typ> for $typ {
            fn sub_assign(&mut self, rhs: Self) {
                self.0 -= rhs.0;
            }
        }
        impl std::ops::Add<usize> for $typ {
            type Output = $typ;
            fn add(self, rhs: usize) -> Self::Output {
                Self(self.0 + rhs)
            }
        }
        impl std::ops::Sub<usize> for $typ {
            type Output = $typ;
            fn sub(self, rhs: usize) -> Self::Output {
                Self(self.0 - rhs)
            }
        }
        impl std::ops::AddAssign<usize> for $typ {
            fn add_assign(&mut self, rhs: usize) {
                self.0 += rhs;
            }
        }
        impl std::ops::SubAssign<usize> for $typ {
            fn sub_assign(&mut self, rhs: usize) {
                self.0 -= rhs;
            }
        }
        impl Into<usize> for $typ {
            fn into(self) -> usize {
                self.0
            }
        }
        impl From<usize> for $typ {
            fn from(value: usize) -> Self {
                Self(value)
            }
        }
        impl std::fmt::Display for $typ {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}
pub(crate) use impl_id;


// Helper function for running test code.
fn run_code(code: impl FnOnce() -> Vec<Stmt>) {
    debug::init_counter();
    let stmts = code();

    println!("Type checking...");
    let mut type_visitor = TypeVisitor::new();
    for s in stmts.iter() {
        match type_visitor.visit_stmt(s) {
            // almost always this is unit so its not actually that useful
            Ok(_) => {}
            Err(e) => {
                println!("{}", e);
                return;
            }
        }
    }
    for (id, ty) in type_visitor.ctxt.bindings().clone().into_iter() {
        let mut str = String::new();
        let _ = type_visitor.ctxt.write_ty(&ty, &mut str);
        println!("val {}: {}", id, str);
    }
    println!("Type checking complete.\n");

    println!("Compiling...");

    let mut compiler = Compiler::new(type_visitor.export());
    for s in stmts.iter() {
        compiler.visit_stmt(s).unwrap();
    }
    // this might be bad
    let source = compiler.build();

    println!("Printing source file.\n{}", source);

    println!("Executing...");

    let mut vm = TailVirtualMachine::new(&source);

    let now = Instant::now();
    vm.run();
    let elapsed = now.elapsed().as_secs_f64() * 1000.0;
    vm._print_state();
    println!("Ran in {:.3}ms", elapsed);
}

#[derive(clap::Parser, Debug)]
struct CliConfig {
    /// Sets the test function to run. Defaults to slides_code
    #[arg(short, long, default_value = "slides_code")]
    test: String
}



fn main() {

    let config: CliConfig = <CliConfig as clap::Parser>::parse().into();

    let to_run = match config.test.as_str() {
        "slides_code" => debug::slides_code,
        "factorial" => debug::factorial,
        "bad_fib" => debug::bad_fib,
        "basic_closure" => debug::basic_closure,
        "complex_closure" => debug::complex_closure,
        "basic_refs" => debug::basic_refs,
        "unit_values" => debug::unit_values,
        s => panic!("Unknown test function: {}", s)
    };
    println!("Running {}\n\n", config.test);
    run_code(to_run);
}