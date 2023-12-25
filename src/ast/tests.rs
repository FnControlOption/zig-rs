use super::*;
use crate::ast::node::ExtraData;
use crate::ast::GetExtraData;
use crate::macros::{node, token};

// After changing DUMP_TREE to true, run `cargo test` with `--show-output`:
//
//     cargo test --quiet ast::tests -- --show-output
//
const DUMP_TREE: bool = false;

macro_rules! assert_node {
    ($tree:ident, $index:expr, $tag:ident) => {{
        let node = $tree.node($index);
        assert_eq!(node.tag, node!($tag));
        node
    }};
}

macro_rules! assert_token {
    ($tree:ident, $index:expr, $tag:ident) => {{
        let tag = $tree.token_tag($index);
        assert_eq!(tag, token!($tag));
        tag
    }};
}

#[track_caller]
fn parse(source: &str, mode: Mode) -> (Ast, node::Index, node::Index) {
    let tree = Ast::parse(source.as_bytes(), mode);
    if DUMP_TREE {
        println!();
        println!("{}", std::panic::Location::caller());
        println!("{source}");
        print!("{tree:?}");
    }
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
    assert_token!(tree, node.main_token, NumberLiteral);

    let (tree, index) = parse_zon("3.14");
    let node = assert_node!(tree, index, NumberLiteral);
    assert_token!(tree, node.main_token, NumberLiteral);
}

#[test]
fn test_zon_char_literal() {
    let (tree, index) = parse_zon("'?'");
    let node = assert_node!(tree, index, CharLiteral);
    assert_token!(tree, node.main_token, CharLiteral);
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

    let (tree, index) = parse_zig("const foo = 42;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    assert_token!(tree, node.main_token, KeywordConst);
    assert_eq!(node.data.lhs, 0);
    assert_node!(tree, node.data.rhs, NumberLiteral);
}

#[test]
fn test_empty_container_decl() {
    let (tree, index) = parse_zig("const Foo = struct {};");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ContainerDeclTwo);
    assert_token!(tree, node.main_token, KeywordStruct);
    assert_eq!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);

    let (tree, index) = parse_zig("const Foo = union {};");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ContainerDeclTwo);
    assert_token!(tree, node.main_token, KeywordUnion);
    assert_eq!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);

    let (tree, index) = parse_zig("const Foo = opaque {};");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ContainerDeclTwo);
    assert_token!(tree, node.main_token, KeywordOpaque);
    assert_eq!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);

    let (tree, index) = parse_zig("const Foo = enum {};");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, ContainerDeclTwo);
    assert_token!(tree, node.main_token, KeywordEnum);
    assert_eq!(node.data.lhs, 0);
    assert_eq!(node.data.rhs, 0);
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
fn test_identifier() {
    let (tree, index) = parse_zig("const foo = bar;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, Identifier);
    assert_token!(tree, node.main_token, Identifier);
}

#[test]
fn test_container_field() {
    let (tree, index) = parse_zig("foo: Foo");
    let node = assert_node!(tree, index, ContainerFieldInit);
    assert_token!(tree, node.main_token, Identifier);
    assert_node!(tree, node.data.lhs, Identifier);
    assert_eq!(node.data.rhs, 0);
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
    assert_token!(tree, node.data.lhs, StringLiteral);
    assert_node!(tree, node.data.rhs, BlockTwo);
}

#[test]
fn test_call_one_0() {
    let (tree, index) = parse_zig("const _ = foo();");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, CallOne);
    assert_token!(tree, node.main_token, LParen);
    assert_node!(tree, node.data.lhs, Identifier);
    assert_eq!(node.data.rhs, 0);
}

#[test]
fn test_call_one_1() {
    let (tree, index) = parse_zig("const _ = foo(a);");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, CallOne);
    assert_token!(tree, node.main_token, LParen);
    assert_node!(tree, node.data.lhs, Identifier);
    assert_node!(tree, node.data.rhs, Identifier);
}

#[test]
fn test_call() {
    let (tree, index) = parse_zig("const _ = foo(a, b);");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, Call);
    assert_token!(tree, node.main_token, LParen);
    assert_node!(tree, node.data.lhs, Identifier);
    let range: node::SubRange = tree.extra_data(node.data.rhs);
    assert_eq!(range.end - range.start, 2);

    let (tree, index) = parse_zig("const _ = foo(a, b, c);");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, Call);
    assert_token!(tree, node.main_token, LParen);
    assert_node!(tree, node.data.lhs, Identifier);
    let range: node::SubRange = tree.extra_data(node.data.rhs);
    assert_eq!(range.end - range.start, 3);
}

#[test]
fn test_fn_proto_simple_0() {
    let (tree, index) = parse_zig("fn foo() void {}");
    let decl = assert_node!(tree, index, FnDecl);
    let proto = assert_node!(tree, decl.data.lhs, FnProtoSimple);
    assert_token!(tree, proto.main_token, KeywordFn);
    assert_eq!(proto.data.lhs, 0);
    assert_node!(tree, proto.data.rhs, Identifier);
    assert_node!(tree, decl.data.rhs, BlockTwo);
}

#[test]
fn test_fn_proto_simple_1() {
    let (tree, index) = parse_zig("fn foo(a: A) void {}");
    let decl = assert_node!(tree, index, FnDecl);
    let proto = assert_node!(tree, decl.data.lhs, FnProtoSimple);
    assert_token!(tree, proto.main_token, KeywordFn);
    assert_node!(tree, proto.data.lhs, Identifier);
    assert_node!(tree, proto.data.rhs, Identifier);
    assert_node!(tree, decl.data.rhs, BlockTwo);
}

#[test]
fn test_fn_proto_multi() {
    let (tree, index) = parse_zig("fn foo(a: A, b: B) void {}");
    let decl = assert_node!(tree, index, FnDecl);
    let proto = assert_node!(tree, decl.data.lhs, FnProtoMulti);
    assert_token!(tree, proto.main_token, KeywordFn);
    let range: node::SubRange = tree.extra_data(proto.data.lhs);
    assert_eq!(range.end - range.start, 2);
    assert_node!(tree, proto.data.rhs, Identifier);

    let (tree, index) = parse_zig("fn foo(a: A, b: B, c: C) void {}");
    let decl = assert_node!(tree, index, FnDecl);
    let proto = assert_node!(tree, decl.data.lhs, FnProtoMulti);
    assert_token!(tree, proto.main_token, KeywordFn);
    let range: node::SubRange = tree.extra_data(proto.data.lhs);
    assert_eq!(range.end - range.start, 3);
    assert_node!(tree, proto.data.rhs, Identifier);
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
    assert_token!(tree, node.data.lhs, Identifier);
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
    let (tree, index) = parse_zig("var foo align(bar) = undefined;");
    let node = assert_node!(tree, index, AlignedVarDecl);
    assert_token!(tree, node.main_token, KeywordVar);
    assert_node!(tree, node.data.lhs, Identifier);
    assert_node!(tree, node.data.rhs, Identifier);

    let (tree, index) = parse_zig("const foo align(bar) = undefined;");
    let node = assert_node!(tree, index, AlignedVarDecl);
    assert_token!(tree, node.main_token, KeywordConst);
    assert_node!(tree, node.data.lhs, Identifier);
    assert_node!(tree, node.data.rhs, Identifier);
}

#[test]
fn test_local_var_decl() {
    let (tree, index) = parse_zig("var foo: Foo align(bar) = undefined;");
    let node = assert_node!(tree, index, LocalVarDecl);
    assert_token!(tree, node.main_token, KeywordVar);
    let extra: node::LocalVarDecl = tree.extra_data(node.data.lhs);
    assert_node!(tree, extra.type_node, Identifier);
    assert_node!(tree, extra.align_node, Identifier);
    assert_node!(tree, node.data.rhs, Identifier);

    let (tree, index) = parse_zig("const foo: Foo align(bar) = undefined;");
    let node = assert_node!(tree, index, LocalVarDecl);
    assert_token!(tree, node.main_token, KeywordConst);
    let extra: node::LocalVarDecl = tree.extra_data(node.data.lhs);
    assert_node!(tree, extra.type_node, Identifier);
    assert_node!(tree, extra.align_node, Identifier);
    assert_node!(tree, node.data.rhs, Identifier);
}

#[test]
fn test_global_var_decl() {
    let (tree, index) = parse_zig("var foo linksection(bar) = undefined;");
    let node = assert_node!(tree, index, GlobalVarDecl);
    assert_token!(tree, node.main_token, KeywordVar);
    let extra: node::GlobalVarDecl = tree.extra_data(node.data.lhs);
    assert_eq!(extra.type_node, 0);
    assert_eq!(extra.align_node, 0);
    assert_eq!(extra.addrspace_node, 0);
    assert_node!(tree, extra.section_node, Identifier);
    assert_node!(tree, node.data.rhs, Identifier);

    let (tree, index) = parse_zig("const foo addrspace(bar) = undefined;");
    let node = assert_node!(tree, index, GlobalVarDecl);
    assert_token!(tree, node.main_token, KeywordConst);
    let extra: node::GlobalVarDecl = tree.extra_data(node.data.lhs);
    assert_eq!(extra.type_node, 0);
    assert_eq!(extra.align_node, 0);
    assert_node!(tree, extra.addrspace_node, Identifier);
    assert_eq!(extra.section_node, 0);
    assert_node!(tree, node.data.rhs, Identifier);
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
    assert_node!(tree, node.data.lhs, Identifier);
    assert_token!(tree, node.data.rhs, Identifier);
}

#[test]
fn test_var_decl_in_block() {
    let (tree, index) = parse_zig("const _ = { var a = foo; };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwoSemicolon);
    let node = assert_node!(tree, node.data.lhs, SimpleVarDecl);
    assert_token!(tree, node.main_token, KeywordVar);
    assert_eq!(node.data.lhs, 0);
    assert_node!(tree, node.data.rhs, Identifier);
}

#[test]
fn test_assign_destructure() {
    let (tree, index) = parse_zig("const _ = { var a, const b = foo; };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwoSemicolon);
    let node = assert_node!(tree, node.data.lhs, AssignDestructure);
    assert_token!(tree, node.main_token, Equal);
    let lhs_count: node::Index = tree.extra_data(node.data.lhs);
    assert_eq!(lhs_count, 2);
    assert_node!(tree, node.data.rhs, Identifier);
}

#[test]
fn test_ptr_type_aligned() {
    let (tree, index) = parse_zig("const foo: []Foo = undefined;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.lhs, PtrTypeAligned);
    assert_token!(tree, node.main_token, LBracket);
    assert_eq!(node.data.lhs, 0);
    assert_node!(tree, node.data.rhs, Identifier);

    let (tree, index) = parse_zig("const foo: []align(bar) Foo = undefined;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.lhs, PtrTypeAligned);
    assert_token!(tree, node.main_token, LBracket);
    assert_node!(tree, node.data.lhs, Identifier);
    assert_node!(tree, node.data.rhs, Identifier);
}

#[test]
fn test_array_type() {
    let (tree, index) = parse_zig("const foo: [bar]Foo = undefined;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.lhs, ArrayType);
    assert_token!(tree, node.main_token, LBracket);
    assert_node!(tree, node.data.lhs, Identifier);
    assert_node!(tree, node.data.rhs, Identifier);
}

#[test]
fn test_array_type_sentinel() {
    let (tree, index) = parse_zig("const foo: [bar:baz]Foo = undefined;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.lhs, ArrayTypeSentinel);
    assert_token!(tree, node.main_token, LBracket);
    assert_node!(tree, node.data.lhs, Identifier);
    let extra: node::ArrayTypeSentinel = tree.extra_data(node.data.rhs);
    assert_node!(tree, extra.elem_type, Identifier);
    assert_node!(tree, extra.sentinel, Identifier);
}

#[test]
fn test_ptr_type_sentinel() {
    let (tree, index) = parse_zig("const foo: [:bar]Foo = undefined;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.lhs, PtrTypeSentinel);
    assert_token!(tree, node.main_token, LBracket);
    assert_node!(tree, node.data.lhs, Identifier);
    assert_node!(tree, node.data.rhs, Identifier);
}

#[test]
fn test_ptr_type() {
    let (tree, index) = parse_zig("const foo: [:bar]align(baz) Foo = undefined;");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.lhs, PtrType);
    assert_token!(tree, node.main_token, LBracket);
    let extra: node::PtrType = tree.extra_data(node.data.lhs);
    assert_node!(tree, extra.sentinel, Identifier);
    assert_node!(tree, extra.align_node, Identifier);
    assert_eq!(extra.addrspace_node, 0);
    assert_node!(tree, node.data.rhs, Identifier);
}

#[test]
fn test_fn_proto_one() {
    let (tree, index) = parse_zig("fn foo(a: A) addrspace(bar) void {}");
    let node = assert_node!(tree, index, FnDecl);
    let node = assert_node!(tree, node.data.lhs, FnProtoOne);
    assert_token!(tree, node.main_token, KeywordFn);
    let extra: node::FnProtoOne = tree.extra_data(node.data.lhs);
    assert_node!(tree, extra.param, Identifier);
    assert_eq!(extra.align_expr, 0);
    assert_node!(tree, extra.addrspace_expr, Identifier);
    assert_eq!(extra.section_expr, 0);
    assert_eq!(extra.callconv_expr, 0);
    assert_node!(tree, node.data.rhs, Identifier);
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
    assert_node!(tree, extra.addrspace_expr, Identifier);
    assert_eq!(extra.section_expr, 0);
    assert_eq!(extra.callconv_expr, 0);
    assert_node!(tree, node.data.rhs, Identifier);
}

#[test]
fn test_asm_simple() {
    let (tree, index) = parse_zig("const _ = { asm(foo); };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwoSemicolon);
    let node = assert_node!(tree, node.data.lhs, AsmSimple);
    assert_token!(tree, node.main_token, KeywordAsm);
    assert_node!(tree, node.data.lhs, Identifier);
    assert_token!(tree, node.data.rhs, RParen);
}

#[test]
fn test_asm() {
    let (tree, index) = parse_zig("const _ = { asm(foo : ); };");
    let node = assert_node!(tree, index, SimpleVarDecl);
    let node = assert_node!(tree, node.data.rhs, BlockTwoSemicolon);
    let node = assert_node!(tree, node.data.lhs, Asm);
    assert_token!(tree, node.main_token, KeywordAsm);
    assert_node!(tree, node.data.lhs, Identifier);
    let extra: node::Asm = tree.extra_data(node.data.rhs);
    assert_eq!(extra.items_end - extra.items_start, 0);
    assert_token!(tree, extra.rparen, RParen);
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
