#![allow(unused_variables)]

use zig::*;

fn main() {
    let home = std::env::var("HOME").unwrap();
    let path = format!("{home}/Documents/zig/lib/std/zig/Parse.zig");
    let source = std::fs::read_to_string(path).unwrap();
    let source = r#"const _ = for (a, b) |c, d| {};"#;
    let tree = Ast::parse(source.as_bytes(), ast::Mode::Zig);
    for error in tree.errors.iter() {
        println!("{:?}", error.tag);
    }
    if tree.errors.is_empty() {
        print!("{tree:?}");
    }
}
