#![allow(dead_code)]
#![allow(unreachable_code)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_variables)]

pub mod token;
pub use token::{Token, Tokenizer};

pub mod ast;
pub use ast::Ast;

mod macros;
mod parse;

fn main() {
    let source = r#"const _ = foo.bar;"#;
    let source = r#"const _ = { var a = foo; };"#;
    let source = r#"const _ = { var a, const b = foo; };"#;
    let source = r#"const foo: []Foo = undefined;"#;
    let source = r#"const foo: []align(bar) Foo = undefined;"#;
    let source = r#"const foo: [bar]Foo = undefined;"#;
    let source = r#"const foo: [bar:baz]Foo = undefined;"#;
    let source = r#"const foo: [:bar]Foo = undefined;"#;
    let source = r#"const foo: [:bar]align(baz) Foo = undefined;"#;
    let source = r#"fn foo(a: A) addrspace(bar) void {}"#;
    let source = r#"fn foo(a: A, b: B) addrspace(bar) void {}"#;
    let source = r#"asm"#;
    let tree = Ast::parse(source.as_bytes(), ast::Mode::Zig);
    for error in tree.errors.iter() {
        println!("{:?}", error.tag);
    }
    if tree.errors.is_empty() {
        print!("{tree:?}");
    }
}
