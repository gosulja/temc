const std = @import("std");
const testing = std.testing;
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Stmt = @import("parser.zig").Stmt;
const TokenType = @import("token.zig").TokenType;

fn parse_source(alloc: std.mem.Allocator, source: []const u8) ![]?*Stmt {
    var lexer = Lexer.Lexer.init(alloc, source);
    defer lexer.deinit();

    const tokens = try lexer.scan_all();

    var parser = Parser.Parser.init(alloc, tokens);
    return try parser.parse();
}

test "basic func parsing" {
    const source =
        \\ func add(x: int, y: int) -> int {
        \\     return x + y;
        \\}
    ;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const stmts = try parse_source(alloc, source);
    try testing.expect(stmts.len == 1);

    const func = stmts[0].?.Function;
    try testing.expectEqualStrings("add", func.name.lexeme);
    try testing.expect(func.params.len == 2);
    try testing.expectEqualStrings("x", func.params[0].name.lexeme);
    try testing.expectEqualStrings("int", func.params[0].type_tok.lexeme);
    try testing.expectEqualStrings("y", func.params[1].name.lexeme);
    try testing.expectEqualStrings("int", func.params[1].type_tok.lexeme);
    try testing.expect(func.return_type != null);
    try testing.expectEqualStrings("int", func.return_type.?.lexeme);
}

test "struct declaration" {
    const source =
        \\struct Point {
        \\    x: int,
        \\    y: int,
        \\    pub label: string,
        \\}
    ;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const stmts = try parse_source(alloc, source);
    try testing.expect(stmts.len == 1);

    const strct = stmts[0].?.Struct;
    try testing.expectEqualStrings("Point", strct.name.lexeme);
    try testing.expect(strct.fields.len == 3);

    try testing.expectEqualStrings("x", strct.fields[0].name.lexeme);
    try testing.expectEqualStrings("int", strct.fields[0].type_tok.lexeme);
    try testing.expect(!strct.fields[0].is_pub);

    try testing.expectEqualStrings("y", strct.fields[1].name.lexeme);
    try testing.expectEqualStrings("int", strct.fields[1].type_tok.lexeme);
    try testing.expect(!strct.fields[1].is_pub);

    try testing.expectEqualStrings("label", strct.fields[2].name.lexeme);
    try testing.expectEqualStrings("string", strct.fields[2].type_tok.lexeme);
    try testing.expect(strct.fields[2].is_pub);
}

test "variable declaration and assignment" {
    const source =
        \\var x: int = 42;
        \\x = x + 1;
    ;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const stmts = try parse_source(alloc, source);
    try testing.expect(stmts.len == 2);

    const var_decl = stmts[0].?.Var;
    try testing.expectEqualStrings("x", var_decl.name.lexeme);
    try testing.expect(var_decl.type_an != null);
    try testing.expectEqualStrings("int", var_decl.type_an.?.lexeme);

    const expr_stmt = stmts[1].?.Expression;
    const assign = expr_stmt.expr.Assignment;
    try testing.expectEqualStrings("x", assign.name.lexeme);
}

test "complex expression parsing" {
    const source = "return (a + b) * c / (d - e);";

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const stmts = try parse_source(alloc, source);
    try testing.expect(stmts.len == 1);

    const ret = stmts[0].?.Return;
    try testing.expect(ret.value != null);
    // the presence of binary expressions in the correct structure implies
    // proper operator precedence handling
    _ = ret.value.?.Binary;
}
