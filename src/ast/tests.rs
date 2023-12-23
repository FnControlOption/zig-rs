use super::*;

fn parse(source: &str, mode: Mode) -> (Ast, node::Index, node::Index) {
    let tree = Ast::parse(source.as_bytes(), mode);
    assert!(tree.errors.is_empty());

    let root = tree.node(0);
    assert_eq!(root.tag, node::Tag::Root);

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
    let node = tree.node(index);
    assert_eq!(node.tag, node::Tag::StructInitDotTwo);
    assert_eq!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_zon_number_literal() {
    let (tree, index) = parse_zon("42");
    let node = tree.node(index);
    assert_eq!(node.tag, node::Tag::NumberLiteral);

    let (tree, index) = parse_zon("3.14");
    let node = tree.node(index);
    assert_eq!(node.tag, node::Tag::NumberLiteral);
}

#[test]
fn test_zon_char_literal() {
    let (tree, index) = parse_zon("'?'");
    let node = tree.node(index);
    assert_eq!(node.tag, node::Tag::CharLiteral);
}

#[test]
fn test_zon_struct_init_with_one_field() {
    let (tree, index) = parse_zon(".{ .foo = 42 }");
    let node = tree.node(index);
    assert_eq!(node.tag, node::Tag::StructInitDotTwo);
    assert_ne!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_simple_var_decl() {
    let (tree, index) = parse_zig("var foo = 42;");
    let node = tree.node(index);
    assert_eq!(node.tag, node::Tag::SimpleVarDecl);
    assert_eq!(node.data.lhs, 0);
    assert_ne!(node.data.rhs, 0);

    let (tree, index) = parse_zig("const foo = 42;");
    let node = tree.node(index);
    assert_eq!(node.tag, node::Tag::SimpleVarDecl);
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
        let node = tree.node(index);
        assert_eq!(node.tag, node::Tag::SimpleVarDecl);
        let node = tree.node(node.data.rhs);
        assert_eq!(node.tag, node::Tag::ContainerDeclTwo);
        assert_eq!(node.data.lhs, 0);
        assert_eq!(node.data.rhs, 0);
    }
}
