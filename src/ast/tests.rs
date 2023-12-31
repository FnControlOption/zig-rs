use super::*;

// After changing DUMP_TREE to true, run `cargo test` with `--show-output`:
//
//     cargo test ast::tests -- --show-output
//
const DUMP_TREE: bool = false;

macro_rules! assert_node {
    ($tree:ident, $index:expr, $tag:ident) => {{
        let node = $tree.node($index);
        assert_eq!(node.tag, N::$tag);
        node
    }};

    // Edge case: "Identifier" is pretty ambiguous, so let's also check the main token
    ($tree:ident, $index:expr, Identifier, $needle:expr) => {{
        let node = assert_node!($tree, $index, Identifier);
        assert_token!($tree, node.main_token, Identifier, $needle);
        node
    }};
}

macro_rules! assert_token {
    ($tree:ident, $index:expr, $tag:ident, $needle:expr) => {{
        assert_eq!($tree.token_tag($index), T::$tag);
        let start = $tree.token_start($index);
        assert!($tree.source(start).starts_with($needle.as_bytes()));
    }};
    ($tree:ident, $index:expr, $tag:ident) => {{
        assert!(T::$tag.lexeme().is_some());
        assert_token!($tree, $index, $tag, T::$tag.symbol());
    }};
}

macro_rules! assert_error {
    ($tree:ident, $index:expr, $tag:ident, is_note: $is_note:expr) => {{
        let error = &$tree.errors[$index];
        assert_eq!(error.tag, E::$tag);
        assert_eq!(error.is_note, $is_note);
        error
    }};
    ($tree:ident, $index:expr, $tag:ident) => {
        assert_error!($tree, $index, $tag, is_note: false)
    };
    ($tree:ident, $index:expr, $tag:ident(_), is_note: $is_note:expr) => {{
        let error = &$tree.errors[$index];
        assert!(matches!(error.tag, E::$tag(_)));
        assert_eq!(error.is_note, $is_note);
        error
    }};
    ($tree:ident, $index:expr, $tag:ident(_)) => {
        assert_error!($tree, $index, $tag(_), is_note: false)
    };
}

#[track_caller]
fn parse_recoverable(source: &str, mode: Mode) -> Ast {
    let tree = Ast::parse(source.as_bytes(), mode);
    if DUMP_TREE {
        println!();
        println!("{}", std::panic::Location::caller());
        println!("{source}");
        print!("{}", tree.display());
    }
    tree
}

#[track_caller]
fn parse(source: &str, mode: Mode) -> (Ast, node::Index, node::Index) {
    let tree = parse_recoverable(source, mode);
    assert!(tree.errors.is_empty());

    let root = assert_node!(tree, 0, Root);
    let lhs = root.data.lhs;
    let rhs = root.data.rhs;
    (tree, lhs, rhs)
}

#[track_caller]
fn parse_zig(source: &str) -> (Ast, node::Index) {
    let (tree, start, end) = parse(source, Mode::Zig);
    assert_eq!(end - start, 1);

    let index = tree.extra_data(start);
    (tree, index)
}

#[track_caller]
fn parse_zon(source: &str) -> (Ast, node::Index) {
    let (tree, index, _) = parse(source, Mode::Zon);
    (tree, index)
}

#[test]
fn test_zon_empty_struct_init() {
    let (tree, index) = parse_zon(".{}");
    let node = assert_node!(tree, index, StructInitDotTwo);
    assert_token!(tree, node.main_token, LBrace);
    assert_eq!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_zon_number_literal() {
    let (tree, index) = parse_zon("42");
    let node = assert_node!(tree, index, NumberLiteral);
    assert_token!(tree, node.main_token, NumberLiteral, "42");

    let (tree, index) = parse_zon("3.14");
    let node = assert_node!(tree, index, NumberLiteral);
    assert_token!(tree, node.main_token, NumberLiteral, "3.14");
}

#[test]
fn test_zon_char_literal() {
    let (tree, index) = parse_zon("'?'");
    let node = assert_node!(tree, index, CharLiteral);
    assert_token!(tree, node.main_token, CharLiteral, "'?'");
}

#[test]
fn test_zon_struct_init_with_one_field() {
    let (tree, index) = parse_zon(".{ .foo = 42 }");
    let node = assert_node!(tree, index, StructInitDotTwo);
    assert_token!(tree, node.main_token, LBrace);
    assert_node!(tree, node.data.lhs, NumberLiteral);
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_simple_var_decl() {
    let (tree, index) = parse_zig("var foo = 42;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    assert_token!(tree, node.main_token, KeywordVar);
    assert_eq!(node.data.lhs, 0);
    assert_node!(tree, node.data.rhs, NumberLiteral);

    let (tree, index) = parse_zig("pub const foo = 42;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    assert_token!(tree, node.main_token - 1, KeywordPub);
    assert_token!(tree, node.main_token, KeywordConst);
    assert_eq!(node.data.lhs, 0);
    assert_node!(tree, node.data.rhs, NumberLiteral);
}

#[test]
fn test_identifier() {
    let (tree, index) = parse_zig("const foo = bar;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    assert_node!(tree, node.data.rhs, Identifier, "bar");
}

#[test]
fn test_container_decl_two() {
    let (tree, index) = parse_zig("const Foo = struct {};");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ContainerDeclTwo);
    assert_token!(tree, node.main_token, KeywordStruct);
    assert_eq!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_container_decl_two_trailing() {
    let (tree, index) = parse_zig("const Foo = struct { const a = 1; };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ContainerDeclTwoTrailing);
    assert_token!(tree, node.main_token, KeywordStruct);
    assert_node!(tree, node.data.lhs, SimpleVarDecl);
    assert_eq!(node.data.rhs, 0);

    let (tree, index) = parse_zig("const Foo = union { const a = 1; const b = 2; };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ContainerDeclTwoTrailing);
    assert_token!(tree, node.main_token, KeywordUnion);
    assert_node!(tree, node.data.lhs, SimpleVarDecl);
    assert_node!(tree, node.data.rhs, SimpleVarDecl);
}

#[test]
fn test_container_decl() {
    let (tree, index) =
        parse_zig("const Foo = opaque { const a = 1; const b = 2; fn c() void {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ContainerDecl);
    assert_token!(tree, node.main_token, KeywordOpaque);
    assert_eq!(node.data.rhs - node.data.lhs, 3);
}

#[test]
fn test_container_decl_trailing() {
    let (tree, index) = parse_zig("const Foo = enum { const a = 1; const b = 2; const c = 3; };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ContainerDeclTrailing);
    assert_token!(tree, node.main_token, KeywordEnum);
    assert_eq!(node.data.rhs - node.data.lhs, 3);
}

#[test]
fn test_container_decl_arg() {
    let (tree, index) = parse_zig("const Foo = struct(Bar) {};");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ContainerDeclArg);
    assert_token!(tree, node.main_token, KeywordStruct);
    assert_node!(tree, node.data.lhs, Identifier, "Bar");
    let extra: node::SubRange = tree.extra_data(node.data.rhs);
    assert_eq!(extra.end - extra.start, 0);
}

#[test]
fn test_container_decl_arg_trailing() {
    let (tree, index) = parse_zig("const Foo = enum(Bar) { const baz = 42; };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ContainerDeclArgTrailing);
    assert_token!(tree, node.main_token, KeywordEnum);
    assert_node!(tree, node.data.lhs, Identifier, "Bar");
    let extra: node::SubRange = tree.extra_data(node.data.rhs);
    assert_eq!(extra.end - extra.start, 1);
}

#[test]
fn test_empty_error_set_decl() {
    let (tree, index) = parse_zig("const Foo = error {};");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ErrorSetDecl);
    assert_token!(tree, node.main_token, KeywordError);
    // lhs is unused
    assert_token!(tree, node.data.rhs, RBrace);
}

#[test]
fn test_usingnamespace() {
    let (tree, index) = parse_zig("usingnamespace foo;");
    let node = assert_node!(tree, index, Usingnamespace);
    assert_token!(tree, node.main_token, KeywordUsingnamespace);
    assert_node!(tree, node.data.lhs, Identifier, "foo");
    // rhs is unused
}

#[test]
fn test_container_field_init() {
    let (tree, index) = parse_zig("foo: Foo = .{}");
    let node = assert_node!(tree, index, ContainerFieldInit);
    assert_token!(tree, node.main_token, Identifier, "foo");
    assert_node!(tree, node.data.lhs, Identifier, "Foo");
    assert_node!(tree, node.data.rhs, StructInitDotTwo);

    let (tree, index) = parse_zig("Foo = .{}");
    let node = assert_node!(tree, index, ContainerFieldInit);
    assert_token!(tree, node.main_token, Identifier, "Foo");
    assert_node!(tree, node.data.lhs, Identifier, "Foo");
    assert_node!(tree, node.data.rhs, StructInitDotTwo);
}

#[test]
fn test_container_field_align() {
    let (tree, index) = parse_zig("foo: Foo align(bar)");
    let node = assert_node!(tree, index, ContainerFieldAlign);
    assert_token!(tree, node.main_token, Identifier, "foo");
    assert_node!(tree, node.data.lhs, Identifier, "Foo");
    assert_node!(tree, node.data.rhs, Identifier, "bar");
}

#[test]
fn test_container_field() {
    let (tree, index) = parse_zig("foo: Foo align(bar) = .{}");
    let node = assert_node!(tree, index, ContainerField);
    assert_token!(tree, node.main_token, Identifier, "foo");
    assert_node!(tree, node.data.lhs, Identifier, "Foo");
    let extra: node::ContainerField = tree.extra_data(node.data.rhs);
    assert_node!(tree, extra.align_expr, Identifier, "bar");
    assert_node!(tree, extra.value_expr, StructInitDotTwo);
}

#[test]
fn test_decl_between_fields() {
    macro_rules! assert_decl_between_fields {
        ($source:expr, $tag:ident) => {{
            let tree = parse_recoverable($source, Mode::Zig);
            assert_eq!(tree.errors.len(), 3);
            let error = assert_error!(tree, 0, DeclBetweenFields);
            assert_token!(tree, error.token, $tag);
            let error = assert_error!(tree, 1, PreviousField, is_note: true);
            assert_token!(tree, error.token, Identifier, "a");
            let error = assert_error!(tree, 2, NextField, is_note: true);
            assert_token!(tree, error.token, Identifier, "b");
        }};
    }

    let source = "
a: A,
var foo = 42;
b: B,
";
    assert_decl_between_fields!(source, KeywordVar);

    let source = "
a: A,
pub const foo = 42;
b: B,
";
    assert_decl_between_fields!(source, KeywordConst);

    let source = "
a: A,
usingnamespace foo;
b: B,
";
    assert_decl_between_fields!(source, KeywordUsingnamespace);

    let source = "
a: A,
test {}
b: B,
";
    assert_decl_between_fields!(source, KeywordTest);
}

#[test]
fn test_empty_block() {
    let (tree, index) = parse_zig("const _ = {};");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwo);
    assert_token!(tree, node.main_token, LBrace);
    assert_eq!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_test_decl() {
    let (tree, index) = parse_zig("test {}");
    let node = assert_node!(tree, index, TestDecl);
    assert_token!(tree, node.main_token, KeywordTest);
    assert_eq!(node.data.lhs, 0);
    assert_node!(tree, node.data.rhs, BlockTwo);

    let (tree, index) = parse_zig(r#"test "foo" {}"#);
    let node = assert_node!(tree, index, TestDecl);
    assert_token!(tree, node.main_token, KeywordTest);
    assert_token!(tree, node.data.lhs, StringLiteral, r#""foo""#);
    assert_node!(tree, node.data.rhs, BlockTwo);
}

#[test]
fn test_test_doc_comment() {
    let source = "
/// Foo
test {}
";
    let tree = parse_recoverable(source, Mode::Zig);
    assert_eq!(tree.errors.len(), 1);
    let error = assert_error!(tree, 0, TestDocComment);
    assert_token!(tree, error.token, DocComment, "/// Foo");
}

#[test]
fn test_call_one_0() {
    let (tree, index) = parse_zig("const _ = foo();");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, CallOne);
    assert_token!(tree, node.main_token, LParen);
    assert_node!(tree, node.data.lhs, Identifier, "foo");
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_call_one_1() {
    let (tree, index) = parse_zig("const _ = foo(a);");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, CallOne);
    assert_token!(tree, node.main_token, LParen);
    assert_node!(tree, node.data.lhs, Identifier, "foo");
    assert_node!(tree, node.data.rhs, Identifier, "a");
}

#[test]
fn test_call() {
    let (tree, index) = parse_zig("const _ = foo(a, b);");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, Call);
    assert_token!(tree, node.main_token, LParen);
    assert_node!(tree, node.data.lhs, Identifier, "foo");
    let extra: node::SubRange = tree.extra_data(node.data.rhs);
    assert_eq!(extra.end - extra.start, 2);

    let (tree, index) = parse_zig("const _ = foo(a, b, c);");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, Call);
    assert_token!(tree, node.main_token, LParen);
    assert_node!(tree, node.data.lhs, Identifier, "foo");
    let extra: node::SubRange = tree.extra_data(node.data.rhs);
    assert_eq!(extra.end - extra.start, 3);
}

#[test]
fn test_fn_proto_simple_0() {
    let (tree, index) = parse_zig("fn foo() void {}");
    let decl = assert_node!(tree, index, FnDecl);
    let proto = assert_node!(tree, decl.data.lhs, FnProtoSimple);
    assert_token!(tree, proto.main_token, KeywordFn);
    assert_eq!(proto.data.lhs, 0);
    assert_node!(tree, proto.data.rhs, Identifier, "void");
    assert_node!(tree, decl.data.rhs, BlockTwo);
}

#[test]
fn test_fn_proto_simple_1() {
    let (tree, index) = parse_zig("fn foo(a: A) void {}");
    let decl = assert_node!(tree, index, FnDecl);
    let proto = assert_node!(tree, decl.data.lhs, FnProtoSimple);
    assert_token!(tree, proto.main_token, KeywordFn);
    assert_node!(tree, proto.data.lhs, Identifier, "A");
    assert_node!(tree, proto.data.rhs, Identifier, "void");
    assert_node!(tree, decl.data.rhs, BlockTwo);
}

#[test]
fn test_fn_proto_multi() {
    let (tree, index) = parse_zig("fn foo(a: A, b: B) void {}");
    let decl = assert_node!(tree, index, FnDecl);
    let proto = assert_node!(tree, decl.data.lhs, FnProtoMulti);
    assert_token!(tree, proto.main_token, KeywordFn);
    let extra: node::SubRange = tree.extra_data(proto.data.lhs);
    assert_eq!(extra.end - extra.start, 2);
    assert_node!(tree, proto.data.rhs, Identifier, "void");

    let (tree, index) = parse_zig("fn foo(a: A, b: B, c: C) void {}");
    let decl = assert_node!(tree, index, FnDecl);
    let proto = assert_node!(tree, decl.data.lhs, FnProtoMulti);
    assert_token!(tree, proto.main_token, KeywordFn);
    let extra: node::SubRange = tree.extra_data(proto.data.lhs);
    assert_eq!(extra.end - extra.start, 3);
    assert_node!(tree, proto.data.rhs, Identifier, "void");
}

#[test]
fn test_block_two_semicolon() {
    let (tree, index) = parse_zig("const _ = { a(); };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwoSemicolon);
    assert_token!(tree, node.main_token, LBrace);
    assert_node!(tree, node.data.lhs, CallOne);
    assert_eq!(node.data.rhs, 0);

    let (tree, index) = parse_zig("const _ = { a(); b(); };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwoSemicolon);
    assert_token!(tree, node.main_token, LBrace);
    assert_node!(tree, node.data.lhs, CallOne);
    assert_node!(tree, node.data.rhs, CallOne);
}

#[test]
fn test_block_semicolon() {
    let (tree, index) = parse_zig("const _ = { a(); b(); c(); };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockSemicolon);
    assert_token!(tree, node.main_token, LBrace);
    assert_eq!(node.data.rhs - node.data.lhs, 3);

    let (tree, index) = parse_zig("const _ = { a(); b(); c(); d(); };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockSemicolon);
    assert_token!(tree, node.main_token, LBrace);
    assert_eq!(node.data.rhs - node.data.lhs, 4);
}

#[test]
fn test_defer() {
    let (tree, index) = parse_zig("const _ = { defer {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwo);
    let node = assert_node!(tree, node.data.lhs, Defer);
    assert_token!(tree, node.main_token, KeywordDefer);
    // lhs is unused
    assert_node!(tree, node.data.rhs, BlockTwo);
}

#[test]
fn test_errdefer() {
    let (tree, index) = parse_zig("const _ = { errdefer {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwo);
    let node = assert_node!(tree, node.data.lhs, Errdefer);
    assert_token!(tree, node.main_token, KeywordErrdefer);
    assert_eq!(node.data.lhs, 0);
    assert_node!(tree, node.data.rhs, BlockTwo);

    let (tree, index) = parse_zig("const _ = { errdefer |foo| {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwo);
    let node = assert_node!(tree, node.data.lhs, Errdefer);
    assert_token!(tree, node.main_token, KeywordErrdefer);
    assert_token!(tree, node.data.lhs, Identifier, "foo");
    assert_node!(tree, node.data.rhs, BlockTwo);
}

#[test]
fn test_block_two() {
    let (tree, index) = parse_zig("const _ = { defer {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwo);
    assert_token!(tree, node.main_token, LBrace);
    assert_node!(tree, node.data.lhs, Defer);
    assert_eq!(node.data.rhs, 0);

    let (tree, index) = parse_zig("const _ = { a(); defer {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwo);
    assert_token!(tree, node.main_token, LBrace);
    assert_node!(tree, node.data.lhs, CallOne);
    assert_node!(tree, node.data.rhs, Defer);
}

#[test]
fn test_block() {
    let (tree, index) = parse_zig("const _ = { a(); b(); defer {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, Block);
    assert_token!(tree, node.main_token, LBrace);
    assert_eq!(node.data.rhs - node.data.lhs, 3);

    let (tree, index) = parse_zig("const _ = { a(); b(); c(); defer {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, Block);
    assert_token!(tree, node.main_token, LBrace);
    assert_eq!(node.data.rhs - node.data.lhs, 4);
}

#[test]
fn test_aligned_var_decl() {
    let (tree, index) = parse_zig("var foo align(bar) = baz;");
    let node = assert_node!(tree, index, AlignedVarDecl);
    assert_token!(tree, node.main_token, KeywordVar);
    assert_node!(tree, node.data.lhs, Identifier, "bar");
    assert_node!(tree, node.data.rhs, Identifier, "baz");

    let (tree, index) = parse_zig("const foo align(bar) = baz;");
    let node = assert_node!(tree, index, AlignedVarDecl);
    assert_token!(tree, node.main_token, KeywordConst);
    assert_node!(tree, node.data.lhs, Identifier, "bar");
    assert_node!(tree, node.data.rhs, Identifier, "baz");
}

#[test]
fn test_local_var_decl() {
    let (tree, index) = parse_zig("var foo: Foo align(bar) = .{};");
    let node = assert_node!(tree, index, LocalVarDecl);
    assert_token!(tree, node.main_token, KeywordVar);
    let extra: node::LocalVarDecl = tree.extra_data(node.data.lhs);
    assert_node!(tree, extra.type_node, Identifier, "Foo");
    assert_node!(tree, extra.align_node, Identifier, "bar");
    assert_node!(tree, node.data.rhs, StructInitDotTwo);

    let (tree, index) = parse_zig("const foo: Foo align(bar) = .{};");
    let node = assert_node!(tree, index, LocalVarDecl);
    assert_token!(tree, node.main_token, KeywordConst);
    let extra: node::LocalVarDecl = tree.extra_data(node.data.lhs);
    assert_node!(tree, extra.type_node, Identifier, "Foo");
    assert_node!(tree, extra.align_node, Identifier, "bar");
    assert_node!(tree, node.data.rhs, StructInitDotTwo);
}

#[test]
fn test_global_var_decl() {
    let (tree, index) = parse_zig("var foo linksection(bar) = baz;");
    let node = assert_node!(tree, index, GlobalVarDecl);
    assert_token!(tree, node.main_token, KeywordVar);
    let extra: node::GlobalVarDecl = tree.extra_data(node.data.lhs);
    assert_eq!(extra.type_node, 0);
    assert_eq!(extra.align_node, 0);
    assert_eq!(extra.addrspace_node, 0);
    assert_node!(tree, extra.section_node, Identifier, "bar");
    assert_node!(tree, node.data.rhs, Identifier, "baz");

    let (tree, index) = parse_zig("const foo addrspace(bar) = baz;");
    let node = assert_node!(tree, index, GlobalVarDecl);
    assert_token!(tree, node.main_token, KeywordConst);
    let extra: node::GlobalVarDecl = tree.extra_data(node.data.lhs);
    assert_eq!(extra.type_node, 0);
    assert_eq!(extra.align_node, 0);
    assert_node!(tree, extra.addrspace_node, Identifier, "bar");
    assert_eq!(extra.section_node, 0);
    assert_node!(tree, node.data.rhs, Identifier, "baz");
}

#[test]
fn test_nosuspend() {
    let (tree, index) = parse_zig("const _ = { nosuspend {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwo);
    let node = assert_node!(tree, node.data.lhs, Nosuspend);
    assert_token!(tree, node.main_token, KeywordNosuspend);
    assert_node!(tree, node.data.lhs, BlockTwo);
    // rhs is unused
}

#[test]
fn test_suspend() {
    let (tree, index) = parse_zig("const _ = { suspend {} };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwo);
    let node = assert_node!(tree, node.data.lhs, Suspend);
    assert_token!(tree, node.main_token, KeywordSuspend);
    assert_node!(tree, node.data.lhs, BlockTwo);
    // rhs is unused
}

#[test]
fn test_field_access() {
    let (tree, index) = parse_zig("const _ = foo.bar;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, FieldAccess);
    assert_token!(tree, node.main_token, Period);
    assert_node!(tree, node.data.lhs, Identifier, "foo");
    assert_token!(tree, node.data.rhs, Identifier, "bar");
}

#[test]
fn test_var_decl_in_block() {
    let (tree, index) = parse_zig("const _ = { var a = foo; };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwoSemicolon);
    let node = assert_node!(tree, node.data.lhs, SimpleVarDecl);
    assert_token!(tree, node.main_token, KeywordVar);
    assert_eq!(node.data.lhs, 0);
    assert_node!(tree, node.data.rhs, Identifier, "foo");
}

#[test]
fn test_assign_destructure() {
    let (tree, index) = parse_zig("const _ = { var a, const b = foo; };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwoSemicolon);
    let node = assert_node!(tree, node.data.lhs, AssignDestructure);
    assert_token!(tree, node.main_token, Equal);
    let lhs_count: u32 = tree.extra_data(node.data.lhs);
    assert_eq!(lhs_count, 2);
    assert_node!(tree, node.data.rhs, Identifier, "foo");
}

#[test]
fn test_ptr_type_aligned() {
    let (tree, index) = parse_zig("const foo: []Foo = undefined;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.lhs, PtrTypeAligned);
    assert_token!(tree, node.main_token, LBracket);
    assert_eq!(node.data.lhs, 0);
    assert_node!(tree, node.data.rhs, Identifier, "Foo");

    let (tree, index) = parse_zig("const foo: []align(bar) Foo = undefined;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.lhs, PtrTypeAligned);
    assert_token!(tree, node.main_token, LBracket);
    assert_node!(tree, node.data.lhs, Identifier, "bar");
    assert_node!(tree, node.data.rhs, Identifier, "Foo");
}

#[test]
fn test_array_type() {
    let (tree, index) = parse_zig("const foo: [bar]Foo = undefined;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.lhs, ArrayType);
    assert_token!(tree, node.main_token, LBracket);
    assert_node!(tree, node.data.lhs, Identifier, "bar");
    assert_node!(tree, node.data.rhs, Identifier, "Foo");
}

#[test]
fn test_array_type_sentinel() {
    let (tree, index) = parse_zig("const foo: [bar:baz]Foo = undefined;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.lhs, ArrayTypeSentinel);
    assert_token!(tree, node.main_token, LBracket);
    assert_node!(tree, node.data.lhs, Identifier, "bar");
    let extra: node::ArrayTypeSentinel = tree.extra_data(node.data.rhs);
    assert_node!(tree, extra.sentinel, Identifier, "baz");
    assert_node!(tree, extra.elem_type, Identifier, "Foo");
}

#[test]
fn test_ptr_type_sentinel() {
    let (tree, index) = parse_zig("const foo: [:bar]Foo = undefined;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.lhs, PtrTypeSentinel);
    assert_token!(tree, node.main_token, LBracket);
    assert_node!(tree, node.data.lhs, Identifier, "bar");
    assert_node!(tree, node.data.rhs, Identifier, "Foo");
}

#[test]
fn test_ptr_type() {
    let (tree, index) = parse_zig("const foo: [:bar]align(baz) Foo = undefined;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.lhs, PtrType);
    assert_token!(tree, node.main_token, LBracket);
    let extra: node::PtrType = tree.extra_data(node.data.lhs);
    assert_node!(tree, extra.sentinel, Identifier, "bar");
    assert_node!(tree, extra.align_node, Identifier, "baz");
    assert_eq!(extra.addrspace_node, 0);
    assert_node!(tree, node.data.rhs, Identifier, "Foo");
}

#[test]
fn test_fn_proto_one() {
    let (tree, index) = parse_zig("fn foo(a: A) addrspace(bar) void {}");
    let node = assert_node!(tree, index, FnDecl);
    let node = assert_node!(tree, node.data.lhs, FnProtoOne);
    assert_token!(tree, node.main_token, KeywordFn);
    let extra: node::FnProtoOne = tree.extra_data(node.data.lhs);
    assert_node!(tree, extra.param, Identifier, "A");
    assert_eq!(extra.align_expr, 0);
    assert_node!(tree, extra.addrspace_expr, Identifier, "bar");
    assert_eq!(extra.section_expr, 0);
    assert_eq!(extra.callconv_expr, 0);
    assert_node!(tree, node.data.rhs, Identifier, "void");
}

#[test]
fn test_fn_proto() {
    let (tree, index) = parse_zig("fn foo(a: A, b: B) addrspace(bar) void {}");
    let node = assert_node!(tree, index, FnDecl);
    let node = assert_node!(tree, node.data.lhs, FnProto);
    assert_token!(tree, node.main_token, KeywordFn);
    let extra: node::FnProto = tree.extra_data(node.data.lhs);
    assert_eq!(extra.params_end - extra.params_start, 2);
    assert_eq!(extra.align_expr, 0);
    assert_node!(tree, extra.addrspace_expr, Identifier, "bar");
    assert_eq!(extra.section_expr, 0);
    assert_eq!(extra.callconv_expr, 0);
    assert_node!(tree, node.data.rhs, Identifier, "void");
}

#[test]
fn test_asm_simple() {
    let (tree, index) = parse_zig("const _ = { asm(foo); };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwoSemicolon);
    let node = assert_node!(tree, node.data.lhs, AsmSimple);
    assert_token!(tree, node.main_token, KeywordAsm);
    assert_node!(tree, node.data.lhs, Identifier, "foo");
    assert_token!(tree, node.data.rhs, RParen);
}

#[test]
fn test_asm() {
    let (tree, index) = parse_zig("const _ = { asm(foo : ); };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwoSemicolon);
    let node = assert_node!(tree, node.data.lhs, Asm);
    assert_token!(tree, node.main_token, KeywordAsm);
    assert_node!(tree, node.data.lhs, Identifier, "foo");
    let extra: node::Asm = tree.extra_data(node.data.rhs);
    assert_eq!(extra.items_end - extra.items_start, 0);
    assert_token!(tree, extra.rparen, RParen);
}

#[test]
fn test_unattached_doc_comment() {
    let source = "/// Foo";
    let tree = parse_recoverable(source, Mode::Zig);
    assert_eq!(tree.errors.len(), 1);
    let error = assert_error!(tree, 0, UnattachedDocComment);
    assert_token!(tree, error.token, DocComment, "/// Foo");
}

#[test]
fn test_c_style_container() {
    let source = "struct Foo {};";
    let tree = parse_recoverable(source, Mode::Zig);
    assert_eq!(tree.errors.len(), 2);
    let error = assert_error!(tree, 0, CStyleContainer(_));
    assert_token!(tree, error.token, Identifier, "Foo");
    let error = assert_error!(tree, 1, ZigStyleContainer(_), is_note: true);
    assert_token!(tree, error.token, Identifier, "Foo");
}

// #[test]
// fn test_() {
//     let (tree, index) = parse_zig("");
//     let node = assert_node!(tree, index, SimpleVarDecl);
//     let node = assert_node!(tree, node.data.rhs, );
//     assert_token!(tree, node.main_token, );
//     assert_eq!(node.data.lhs, 0);
//     assert_node!(tree, node.data.rhs, Identifier);
// }
