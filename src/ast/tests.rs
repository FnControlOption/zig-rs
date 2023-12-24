use super::*;
use crate::{
    ast::node::ExtraData,
    macros::{node, token},
};

macro_rules! assert_node {
    ($tree:ident, $index:expr, $tag:ident) => {{
        let node = $tree.node($index);
        assert_eq!(node.tag, node!($tag));
        node
    }};
}

macro_rules! assert_token {
    ($tree:ident, $index:expr, $tag:ident) => {
        assert_eq!($tree.token_tag($index), token!($tag))
    };
}

fn parse(source: &str, mode: Mode) -> (Ast, node::Index, node::Index) {
    let tree = Ast::parse(source.as_bytes(), mode);
    assert!(tree.errors.is_empty());

    let root = assert_node!(tree, 0, Root);
    let lhs = root.data.lhs;
    let rhs = root.data.rhs;
    (tree, lhs, rhs)
}

fn parse_zig(source: &str) -> (Ast, node::Index) {
    let (tree, start, end) = parse(source, Mode::Zig);
    assert_eq!(end - start, 1);

    let index = tree.extra_data(start);
    (tree, index)
}

fn parse_zon(source: &str) -> (Ast, node::Index) {
    let (tree, index, _) = parse(source, Mode::Zon);
    (tree, index)
}

#[test]
fn test_zon_empty_struct_init() {
    let (tree, index) = parse_zon(".{}");
    let node = assert_node!(tree, index, StructInitDotTwo);
    assert_eq!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_zon_number_literal() {
    let (tree, index) = parse_zon("42");
    let _node = assert_node!(tree, index, NumberLiteral);

    let (tree, index) = parse_zon("3.14");
    let _node = assert_node!(tree, index, NumberLiteral);
}

#[test]
fn test_zon_char_literal() {
    let (tree, index) = parse_zon("'?'");
    let _node = assert_node!(tree, index, CharLiteral);
}

#[test]
fn test_zon_struct_init_with_one_field() {
    let (tree, index) = parse_zon(".{ .foo = 42 }");
    let node = assert_node!(tree, index, StructInitDotTwo);
    assert_ne!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_simple_var_decl() {
    let (tree, index) = parse_zig("var foo = 42;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    assert_eq!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);

    let (tree, index) = parse_zig("const foo = 42;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    assert_eq!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);
}

#[test]
fn test_empty_container_decl() {
    for source in [
        "const Foo = struct {};",
        "const Foo = union {};",
        "const Foo = opaque {};",
        "const Foo = enum {};",
    ] {
        let (tree, index) = parse_zig(source);
        let node = assert_node!(tree, index, SimpleVarDecl);
        let node = assert_node!(tree, node.data.rhs, ContainerDeclTwo);
        assert_eq!(node.data.lhs, 0);
        assert_eq!(node.data.rhs, 0);
    }
}

#[test]
fn test_empty_error_set_decl() {
    let (tree, index) = parse_zig("const Foo = error {};");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ErrorSetDecl);
    // lhs is unused
    assert_token!(tree, node.data.rhs, RBrace);
}

#[test]
fn test_identifier() {
    let (tree, index) = parse_zig("const foo = bar;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let _node = assert_node!(tree, node.data.rhs, Identifier);
}

#[test]
fn test_container_field() {
    let (tree, index) = parse_zig("foo: Foo");
    let node = assert_node!(tree, index, ContainerFieldInit);
    assert_ne!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_empty_block() {
    let (tree, index) = parse_zig("const _ = {};");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwo);
    assert_eq!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_test_decl() {
    let (tree, index) = parse_zig("test {}");
    let node = assert_node!(tree, index, TestDecl);
    assert_eq!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);

    let (tree, index) = parse_zig(r#"test "foo" {}"#);
    let node = assert_node!(tree, index, TestDecl);
    assert_ne!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);
}

#[test]
fn test_call_one_0() {
    let (tree, index) = parse_zig("const _ = foo();");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, CallOne);
    assert_ne!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_call_one_1() {
    let (tree, index) = parse_zig("const _ = foo(a);");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, CallOne);
    assert_ne!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);
}

#[test]
fn test_call() {
    let (tree, index) = parse_zig("const _ = foo(a, b);");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, Call);
    assert_ne!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);
    let range = node::SubRange::from_start(&tree, node.data.rhs);
    assert_eq!(range.end - range.start, 2);

    let (tree, index) = parse_zig("const _ = foo(a, b, c);");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, Call);
    assert_ne!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);
    let range = node::SubRange::from_start(&tree, node.data.rhs);
    assert_eq!(range.end - range.start, 3);
}

#[test]
fn test_fn_proto_simple_0() {
    let (tree, index) = parse_zig("fn foo() void {}");
    let node = assert_node!(tree, index, FnDecl);
    assert_ne!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);

    let node = assert_node!(tree, node.data.lhs, FnProtoSimple);
    assert_eq!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);
}

#[test]
fn test_fn_proto_simple_1() {
    let (tree, index) = parse_zig("fn foo(a: A) void {}");
    let node = assert_node!(tree, index, FnDecl);
    assert_ne!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);

    let node = assert_node!(tree, node.data.lhs, FnProtoSimple);
    assert_ne!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);
}

#[test]
fn test_fn_proto_multi() {
    let (tree, index) = parse_zig("fn foo(a: A, b: B) void {}");
    let node = assert_node!(tree, index, FnDecl);
    let node = assert_node!(tree, node.data.lhs, FnProtoMulti);
    assert_ne!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);

    let range = node::SubRange::from_start(&tree, node.data.lhs);
    assert_eq!(range.end - range.start, 2);

    let (tree, index) = parse_zig("fn foo(a: A, b: B, c: C) void {}");
    let node = assert_node!(tree, index, FnDecl);
    let node = assert_node!(tree, node.data.lhs, FnProtoMulti);
    assert_ne!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);

    let range = node::SubRange::from_start(&tree, node.data.lhs);
    assert_eq!(range.end - range.start, 3);
}

#[test]
fn test_block_two_semicolon() {
    let (tree, index) = parse_zig("const _ = { a(); };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwoSemicolon);
    assert_ne!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);

    let (tree, index) = parse_zig("const _ = { a(); b(); };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwoSemicolon);
    assert_ne!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);
}

#[test]
fn test_block_semicolon() {
    let (tree, index) = parse_zig("const _ = { a(); b(); c(); };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockSemicolon);
    assert_eq!(node.data.rhs - node.data.lhs, 3);

    let (tree, index) = parse_zig("const _ = { a(); b(); c(); d(); };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockSemicolon);
    assert_eq!(node.data.rhs - node.data.lhs, 4);
}

#[test]
fn test_defer() {
    let (tree, index) = parse_zig("const _ = { defer {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwo);
    let node = assert_node!(tree, node.data.lhs, Defer);
    // lhs is unused
    assert_ne!(node.data.rhs, 0);
}

#[test]
fn test_block_two() {
    let (tree, index) = parse_zig("const _ = { defer {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwo);
    assert_ne!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);

    let (tree, index) = parse_zig("const _ = { a(); defer {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwo);
    assert_ne!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);
}

#[test]
fn test_block() {
    let (tree, index) = parse_zig("const _ = { a(); b(); defer {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, Block);
    assert_eq!(node.data.rhs - node.data.lhs, 3);

    let (tree, index) = parse_zig("const _ = { a(); b(); c(); defer {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, Block);
    assert_eq!(node.data.rhs - node.data.lhs, 4);
}

// #[test]
// fn test_() {
//     let (tree, index) = parse_zig("");
//     let node = assert_node!(tree, index, SimpleVarDecl);
//     let node = assert_node!(tree, node.data.rhs, );
//     assert_ne!(node.data.lhs, 0);
//     assert_eq!(node.data.rhs, 0);
// }
