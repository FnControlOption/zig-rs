#![allow(unused_variables)]

use zig::*;

fn main() {
    let home = std::env::var("HOME").unwrap();
    if true {
        let root = format!("{home}/Documents/zig");
        let dir = format!("{root}/lib/std");
        recurse(root, dir).unwrap();
        return;
    }
    let filename = "lib/std/unicode/throughput_test.zig";
    let path = format!("{home}/Documents/zig/{filename}");
    let source = std::fs::read_to_string(&path).unwrap();
    let filename = "-e";
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
    // if tree.errors.is_empty() {
    //     print!("{tree:?}");
    // }
}

fn run<'src>(filename: &str, source: &'src str) -> Ast<'src> {
    let tree = Ast::parse(source.as_bytes(), ast::Mode::Zig);
    for error in tree.errors.iter() {
        tree.dump_error(error, filename);
    }
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
            // empty
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
