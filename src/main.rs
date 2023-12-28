#![allow(unused_variables)]

use zig::*;

fn main() {
    let home = std::env::var("HOME").unwrap();
    let filename = "lib/std/zig/Parse.zig";
    // let filename = "lib/std/zig.zig";
    let path = format!("{home}/Documents/zig/{filename}");
    let source = std::fs::read_to_string(&path).unwrap();
    // let source = r#"const _ = for (a, b) |c, d| {};"#;
    let tree = Ast::parse(source.as_bytes(), ast::Mode::Zig);
    for error in tree.errors.iter() {
        let loc = tree.token_location(0, error.token);
        let line = loc.line + 1;
        let column = loc.column + 1 + tree.error_offset(error) as usize;
        print!("{filename}:{line}:{column}: ");
        tree.render_error(error);
        println!();
    }
    if tree.errors.is_empty() {
        print!("{tree:?}");
    }
}
