#![allow(clippy::all)]
#![allow(dead_code)]
#![allow(unreachable_code)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_variables)]

use zig::*;

fn main() {
    let home = std::env::var("HOME").unwrap();
    if false {
        let root = format!("{home}/Documents/zig");
        let dir = format!("{root}/lib/std");
        let dir = format!("{root}/src");
        let dir = format!("{root}");
        recurse(root, dir).unwrap();
        return;
    }
    let filename = "lib/std/zig/Parse.zig";
    let path = format!("{home}/Documents/zig/{filename}");
    let source = std::fs::read_to_string(&path).unwrap();
    let filename = "-e";
    let source = r#"const foo;"#;
    let source = r#"const _ = "こんにちは";"#;
    if false {
        let mut tokenizer = Tokenizer::new(source.as_bytes());
        loop {
            let token = tokenizer.next();
            println!("{:?}", token.tag);
            if token.tag == token::Tag::Eof {
                break;
            }
        }
    }
    let tree = run(filename, &source);
    if false && tree.errors.is_empty() {
        print!("{tree:?}");
    }
    let bytes = tree.render().unwrap();
    println!("{}", String::from_utf8_lossy(bytes.as_slice()));
}

fn run<'src>(filename: &str, source: &'src str) -> Ast<'src> {
    let tree = Ast::parse(source.as_bytes(), ast::Mode::Zig);
    for error in tree.errors.iter() {
        println!("{}", error.display(filename, &tree));
    }
    format!("{tree:?}");
    tree
}

fn recurse(
    prefix: impl AsRef<std::path::Path>,
    dirpath: impl AsRef<std::path::Path>,
) -> std::io::Result<()> {
    let prefix = prefix.as_ref();
    let dirpath = dirpath.as_ref();

    let entries = std::fs::read_dir(dirpath)?;

    for entry in entries {
        let entry = entry?;
        let meta = entry.metadata()?;

        let path = entry.path();
        let path = path.as_path();

        const SKIP: &[&str] = &[
            "zig-cache",
            // "test/cases/compile_errors",
            "test/cases/double_ampersand.0.zig",
        ];
        let relpath = path.strip_prefix(prefix).unwrap();
        let relname = relpath.to_str().unwrap();
        if SKIP.contains(&relname) {
            continue;
        }

        if meta.is_dir() {
            recurse(prefix, path)?;
        }

        if meta.is_file() && path.extension().map(|ext| ext == "zig").unwrap_or(false) {
            let source = std::fs::read_to_string(path)?;
            run(relname, &source);
        }
    }

    Ok(())
}

#[ignore]
#[test]
fn test() {
    main();
}
