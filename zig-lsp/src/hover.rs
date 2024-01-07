use super::*;

pub fn get_tooltip(
    tree: &Ast,
    pos_context: PositionContext,
    token: TokenIndex,
    node: node::Index,
) -> Option<String> {
    let token_slice = tree.token_slice(token);
    let node_tag = tree.node(node).tag;

    let mut buffer1 = [0; 1];
    let mut buffer2 = [0; 2];

    let (code, doc) = if node_tag == N::Return {
        debug_assert_eq!(b"return", token_slice);
        (token_slice, RETURN_DOC)
    } else if node_tag == N::Identifier {
        match token_slice {
            b"void" => (token_slice, VOID_DOC),
            _ => (token_slice, "TODO(zig-lsp): Add documentation"),
        }
    } else if let Some(var_decl) = tree.full_var_decl(node) {
        if var_decl.visib_token == Some(token) {
            debug_assert_eq!(b"pub", token_slice);
            (token_slice, PUB_DOC)
        } else if var_decl.ast.mut_token == token {
            match token_slice {
                b"const" => (token_slice, CONST_DOC),
                b"var" => (token_slice, VAR_DOC),
                _ => return None,
            }
        } else {
            return None;
        }
    } else if let Some(container_decl) = tree.full_container_decl(&mut buffer2, node) {
        if container_decl.ast.main_token == token {
            match token_slice {
                b"struct" => (token_slice, STRUCT_DOC),
                b"union" => (token_slice, UNION_DOC),
                b"opaque" => (token_slice, OPAQUE_DOC),
                b"enum" => (token_slice, ENUM_DOC),
                _ => return None,
            }
        } else {
            return None;
        }
    } else if let Some(fn_proto) = tree.full_fn_proto(&mut buffer1, node) {
        if fn_proto.visib_token == Some(token) {
            debug_assert_eq!(b"pub", token_slice);
            (token_slice, PUB_DOC)
        } else if fn_proto.ast.fn_token == token {
            debug_assert_eq!(b"fn", token_slice);
            (token_slice, FN_DOC)
        } else {
            return None;
        }
    } else if let Some(builtin_call) = full_builtin_call(tree, &mut buffer2, node) {
        if builtin_call.ast.main_token == token {
            let (code, doc) = match token_slice {
                b"@import" => ("@import(comptime path: []const u8) type", IMPORT_DOC),
                _ => ("", "TODO(zig-lsp): Add documentation"),
            };
            (code.as_bytes(), doc)
        } else {
            return None;
        }
    } else {
        return None;
    };

    let mut value = String::new();
    value.push_str(&format!("```zig\n{}\n```", String::from_utf8_lossy(code)));
    if !doc.is_empty() {
        value.push_str(&format!("\n\n---\n\n{doc}"));
    }
    Some(value)
}

struct BuiltinCall<'buf> {
    ast: BuiltinCallComponents<'buf>,
}

struct BuiltinCallComponents<'buf> {
    main_token: TokenIndex,
    params: &'buf [node::Index],
}

fn full_builtin_call<'ast, 'buf>(
    tree: &'ast Ast,
    buffer: &'buf mut [node::Index; 2],
    node: node::Index,
) -> Option<BuiltinCall<'buf>>
where
    'ast: 'buf,
{
    match tree.node(node).tag {
        N::BuiltinCall | N::BuiltinCallComma => Some(builtin_call_full(tree, node)),
        N::BuiltinCallTwo | N::BuiltinCallTwoComma => Some(builtin_call_one(tree, buffer, node)),
        _ => None,
    }
}

fn builtin_call_one<'ast, 'buf>(
    tree: &'ast Ast,
    buffer: &'buf mut [node::Index; 2],
    node: node::Index,
) -> BuiltinCall<'buf>
where
    'ast: 'buf,
{
    let node::Data { lhs, rhs } = tree.node(node).data;
    *buffer = [lhs, rhs];
    let params = match [lhs, rhs] {
        [0, 0] => &buffer[0..0],
        [_, 0] => &buffer[0..1],
        [_, _] => &buffer[0..2],
    };
    BuiltinCall {
        ast: BuiltinCallComponents {
            main_token: tree.node(node).main_token,
            params,
        },
    }
}

fn builtin_call_full<'ast, 'buf>(tree: &'ast Ast, node: node::Index) -> BuiltinCall<'buf>
where
    'ast: 'buf,
{
    let node::Data { lhs, rhs } = tree.node(node).data;
    BuiltinCall {
        ast: BuiltinCallComponents {
            main_token: tree.node(node).main_token,
            params: tree.extra_data(lhs..rhs),
        },
    }
}

const CONST_DOC: &str = "Declare a variable that cannot be modified.

```zig
const x = 1234;

fn foo() void {
    // It works at file scope as well as inside functions.
    const y = 5678;

    // Once assigned, an identifier cannot be changed.
    y += 1;
}

pub fn main() void {
    foo();
}
```

```sh-session
$ zig build-exe constant_identifier_cannot_change.zig
constant_identifier_cannot_change.zig:8:7: error: cannot assign to constant
    y += 1;
    ~~^~~~
```

`const` applies to all of the bytes that the identifier immediately addresses.
Pointers have their own const-ness.";

const VAR_DOC: &str = r#"Declare a variable that may be modified.

```zig
const print = @import("std").debug.print;

pub fn main() void {
    var y: i32 = 5678;

    y += 1;

    print("{d}", .{y});
}
```

```sh-session
$ zig build-exe mutable_var.zig
$ ./mutable_var
5679
```

Variables must be initialized:

```zig
pub fn main() void {
    var x: i32;

    x = 1;
}
```

```sh-session
$ zig build-exe var_must_be_initialized.zig
var_must_be_initialized.zig:2:15: error: expected '=', found ';'
    var x: i32;
              ^
```"#;

const STRUCT_DOC: &str = r#"Define a struct.

```zig
// Declare a struct.
// Zig gives no guarantees about the order of fields and the size of
// the struct but the fields are guaranteed to be ABI-aligned.
const Point = struct {
    x: f32,
    y: f32,
};

// Maybe we want to pass it to OpenGL so we want to be particular about
// how the bytes are arranged.
const Point2 = packed struct {
    x: f32,
    y: f32,
};


// Declare an instance of a struct.
const p = Point {
    .x = 0.12,
    .y = 0.34,
};

// Maybe we're not ready to fill out some of the fields.
var p2 = Point {
    .x = 0.12,
    .y = undefined,
};

// Structs can have methods
// Struct methods are not special, they are only namespaced
// functions that you can call with dot syntax.
const Vec3 = struct {
    x: f32,
    y: f32,
    z: f32,

    pub fn init(x: f32, y: f32, z: f32) Vec3 {
        return Vec3 {
            .x = x,
            .y = y,
            .z = z,
        };
    }

    pub fn dot(self: Vec3, other: Vec3) f32 {
        return self.x * other.x + self.y * other.y + self.z * other.z;
    }
};

const expect = @import("std").testing.expect;
test "dot product" {
    const v1 = Vec3.init(1.0, 0.0, 0.0);
    const v2 = Vec3.init(0.0, 1.0, 0.0);
    try expect(v1.dot(v2) == 0.0);

    // Other than being available to call with dot syntax, struct methods are
    // not special. You can reference them as any other declaration inside
    // the struct:
    try expect(Vec3.dot(v1, v2) == 0.0);
}

// Structs can have declarations.
// Structs can have 0 fields.
const Empty = struct {
    pub const PI = 3.14;
};
test "struct namespaced variable" {
    try expect(Empty.PI == 3.14);
    try expect(@sizeOf(Empty) == 0);

    // you can still instantiate an empty struct
    const does_nothing = Empty {};

    _ = does_nothing;
}

// struct field order is determined by the compiler for optimal performance.
// however, you can still calculate a struct base pointer given a field pointer:
fn setYBasedOnX(x: *f32, y: f32) void {
    const point = @fieldParentPtr(Point, "x", x);
    point.y = y;
}
test "field parent pointer" {
    var point = Point {
        .x = 0.1234,
        .y = 0.5678,
    };
    setYBasedOnX(&point.x, 0.9);
    try expect(point.y == 0.9);
}

// You can return a struct from a function. This is how we do generics
// in Zig:
fn LinkedList(comptime T: type) type {
    return struct {
        pub const Node = struct {
            prev: ?*Node,
            next: ?*Node,
            data: T,
        };

        first: ?*Node,
        last:  ?*Node,
        len:   usize,
    };
}

test "linked list" {
    // Functions called at compile-time are memoized. This means you can
    // do this:
    try expect(LinkedList(i32) == LinkedList(i32));

    const list = LinkedList(i32){
        .first = null,
        .last = null,
        .len = 0,
    };
    try expect(list.len == 0);

    // Since types are first class values you can instantiate the type
    // by assigning it to a variable:
    const ListOfInts = LinkedList(i32);
    try expect(ListOfInts == LinkedList(i32));

    var node = ListOfInts.Node{
        .prev = null,
        .next = null,
        .data = 1234,
    };
    const list2 = LinkedList(i32){
        .first = &node,
        .last = &node,
        .len = 1,
    };

    // When using a pointer to a struct, fields can be accessed directly,
    // without explicitly dereferencing the pointer.
    // So you can do
    try expect(list2.first.?.data == 1234);
    // instead of try expect(list2.first.?.*.data == 1234);
}
```"#;

const UNION_DOC: &str = r#"Define a union.

A bare `union` defines a set of possible types that a value can be as a list of fields. Only one
field can be active at a time. The in-memory representation of bare unions is not guaranteed. Bare
unions cannot be used to reinterpret memory. For that, use `@ptrCast`, or use an `extern union` or a
`packed union` which have guaranteed in-memory layout. Accessing the non-active field is
safety-checked Undefined Behavior:

```zig
const Payload = union {
    int: i64,
    float: f64,
    boolean: bool,
};
test "simple union" {
    var payload = Payload{ .int = 1234 };
    payload.float = 12.34;
}
```

```sh-session
$ zig test test_wrong_union_access.zig
1/1 test.simple union... thread 1248394 panic: access of union field 'float' while field 'int' is active
```

You can activate another field by assigning the entire union:

```zig
const std = @import("std");
const expect = std.testing.expect;

const Payload = union {
    int: i64,
    float: f64,
    boolean: bool,
};
test "simple union" {
    var payload = Payload{ .int = 1234 };
    try expect(payload.int == 1234);
    payload = Payload{ .float = 12.34 };
    try expect(payload.float == 12.34);
}
```

In order to use `switch` with a union, it must be a tagged union.

To initialize a union when the tag is a comptime-known name, see `@unionInit`."#;

const OPAQUE_DOC: &str = r#"Define an opaque type.

`opaque {}` declares a new type with an unknown (but non-zero) size and alignment. It can contain
declarations the same as structs, unions, and enums.

This is typically used for type safety when interacting with C code that does not expose struct
details. Example:

```zig
const Derp = opaque {};
const Wat = opaque {};

extern fn bar(d: *Derp) void;
fn foo(w: *Wat) callconv(.C) void {
    bar(w);
}

test "call foo" {
    foo(undefined);
}
```

```sh-session
$ zig test test_opaque.zig
test_opaque.zig:6:9: error: expected type '*test_opaque.Derp', found '*test_opaque.Wat'
    bar(w);
        ^
```"#;

const ENUM_DOC: &str = r#"Define an enum type.

```zig
const expect = @import("std").testing.expect;
const mem = @import("std").mem;

// Declare an enum.
const Type = enum {
    ok,
    not_ok,
};

// Declare a specific enum field.
const c = Type.ok;

// If you want access to the ordinal value of an enum, you
// can specify the tag type.
const Value = enum(u2) {
    zero,
    one,
    two,
};
// Now you can cast between u2 and Value.
// The ordinal value starts from 0, counting up by 1 from the previous member.
test "enum ordinal value" {
    try expect(@intFromEnum(Value.zero) == 0);
    try expect(@intFromEnum(Value.one) == 1);
    try expect(@intFromEnum(Value.two) == 2);
}

// You can override the ordinal value for an enum.
const Value2 = enum(u32) {
    hundred = 100,
    thousand = 1000,
    million = 1000000,
};
test "set enum ordinal value" {
    try expect(@intFromEnum(Value2.hundred) == 100);
    try expect(@intFromEnum(Value2.thousand) == 1000);
    try expect(@intFromEnum(Value2.million) == 1000000);
}

// You can also override only some values.
const Value3 = enum(u4) {
    a,
    b = 8,
    c,
    d = 4,
    e,
};
test "enum implicit ordinal values and overridden values" {
    try expect(@intFromEnum(Value3.a) == 0);
    try expect(@intFromEnum(Value3.b) == 8);
    try expect(@intFromEnum(Value3.c) == 9);
    try expect(@intFromEnum(Value3.d) == 4);
    try expect(@intFromEnum(Value3.e) == 5);
}

// Enums can have methods, the same as structs and unions.
// Enum methods are not special, they are only namespaced
// functions that you can call with dot syntax.
const Suit = enum {
    clubs,
    spades,
    diamonds,
    hearts,

    pub fn isClubs(self: Suit) bool {
        return self == Suit.clubs;
    }
};
test "enum method" {
    const p = Suit.spades;
    try expect(!p.isClubs());
}

// An enum can be switched upon.
const Foo = enum {
    string,
    number,
    none,
};
test "enum switch" {
    const p = Foo.number;
    const what_is_it = switch (p) {
        Foo.string => "this is a string",
        Foo.number => "this is a number",
        Foo.none => "this is a none",
    };
    try expect(mem.eql(u8, what_is_it, "this is a number"));
}

// @typeInfo can be used to access the integer tag type of an enum.
const Small = enum {
    one,
    two,
    three,
    four,
};
test "std.meta.Tag" {
    try expect(@typeInfo(Small).Enum.tag_type == u2);
}

// @typeInfo tells us the field count and the fields names:
test "@typeInfo" {
    try expect(@typeInfo(Small).Enum.fields.len == 4);
    try expect(mem.eql(u8, @typeInfo(Small).Enum.fields[1].name, "two"));
}

// @tagName gives a [:0]const u8 representation of an enum value:
test "@tagName" {
    try expect(mem.eql(u8, @tagName(Small.three), "three"));
}
```"#;

const PUB_DOC: &str = "Make a top level declaration available to reference from a different file than the one it is declared in.";

const FN_DOC: &str = r#"Declare a function.

```zig
const std = @import("std");
const builtin = @import("builtin");
const native_arch = builtin.cpu.arch;
const expect = std.testing.expect;

// Functions are declared like this
fn add(a: i8, b: i8) i8 {
    if (a == 0) {
        return b;
    }

    return a + b;
}

// The export specifier makes a function externally visible in the generated
// object file, and makes it use the C ABI.
export fn sub(a: i8, b: i8) i8 { return a - b; }

// The extern specifier is used to declare a function that will be resolved
// at link time, when linking statically, or at runtime, when linking
// dynamically. The quoted identifier after the extern keyword specifies
// the library that has the function. (e.g. "c" -> libc.so)
// The callconv specifier changes the calling convention of the function.
const WINAPI: std.builtin.CallingConvention = if (native_arch == .x86) .Stdcall else .C;
extern "kernel32" fn ExitProcess(exit_code: u32) callconv(WINAPI) noreturn;
extern "c" fn atan2(a: f64, b: f64) f64;

// The @setCold builtin tells the optimizer that a function is rarely called.
fn abort() noreturn {
    @setCold(true);
    while (true) {}
}

// The naked calling convention makes a function not have any function prologue or epilogue.
// This can be useful when integrating with assembly.
fn _start() callconv(.Naked) noreturn {
    abort();
}

// The inline calling convention forces a function to be inlined at all call sites.
// If the function cannot be inlined, it is a compile-time error.
fn shiftLeftOne(a: u32) callconv(.Inline) u32 {
    return a << 1;
}

// The pub specifier allows the function to be visible when importing.
// Another file can use @import and call sub2
pub fn sub2(a: i8, b: i8) i8 { return a - b; }

// Function pointers are prefixed with `*const `.
const Call2Op = *const fn (a: i8, b: i8) i8;
fn doOp(fnCall: Call2Op, op1: i8, op2: i8) i8 {
    return fnCall(op1, op2);
}

test "function" {
    try expect(doOp(add, 5, 6) == 11);
    try expect(doOp(sub2, 5, 6) == -1);
}
```"#;

const RETURN_DOC: &str = "Exit a function with a value.";

const VOID_DOC: &str = r#"Always the value `void{}`

`void` can be useful for instantiating generic types.
For example, given a `Map(Key, Value)`, one can pass `void` for the `Value` type to make it into a `Set`:

```zig
const std = @import("std");
const expect = std.testing.expect;

test "turn HashMap into a set with void" {
    var map = std.AutoHashMap(i32, void).init(std.testing.allocator);
    defer map.deinit();

    try map.put(1, {});
    try map.put(2, {});

    try expect(map.contains(2));
    try expect(!map.contains(3));

    _ = map.remove(2);
    try expect(!map.contains(2));
}
```

Note that this is different from using a dummy value for the hash map value. By using `void` as the
type of the value, the hash map entry type has no value field, and thus the hash map takes up less
space. Further, all the code that deals with storing and loading the value is deleted, as seen above.

`void` is distinct from `anyopaque`. `void` has a known size of 0 bytes, and `anyopaque` has an
unknown, but non-zero, size.

Expressions of type `void` are the only ones whose value can be ignored. For example, ignoring a
non-`void` expression is a compile error:

```zig
test "ignoring expression value" {
    foo();
}

fn foo() i32 {
    return 1234;
}
```

```sh-session
$ zig test test_expression_ignored.zig
test_expression_ignored.zig:2:8: error: value of type 'i32' ignored
    foo();
    ~~~^~
```

However, if the expression has type `void`, there will be no error. Expression results can be
explicitly ignored by assigning them to `_`.

```zig
test "void is ignored" {
    returnsVoid();
}

test "explicitly ignoring expression value" {
    _ = foo();
}

fn returnsVoid() void {}

fn foo() i32 {
    return 1234;
}
```"#;

const IMPORT_DOC: &str = r#"
Find a zig file corresponding to `path` and add it to the build, if it is not already added.

Zig source files are implicitly structs, with a name equal to the file's basename with the extension
truncated. `@import` returns the struct type corresponding to the file.

Declarations which have the `pub` keyword may be referenced from a different source file than the
one they are declared in.

`path` can be a relative path or it can be the name of a package. If it is a relative path, it is
relative to the file that contains the `@import` function call.

The following packages are always available:

- `@import("std")` - Zig Standard Library.
- `@import("builtin")` - Target-specific information.
  The command `zig build-exe --show-builtin` outputs the source to stdout for reference.
- `@import("root")` - Root source file.
  This is usually `src/main.zig` but depends on what file is built.
"#;
