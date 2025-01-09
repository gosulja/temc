const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const TLiteral = @import("token.zig").Literal;

pub const ParError = error{
    IllegalToken,
    InvalidExpression,
    InvalidStatement,
    InvalidAssignment,
    ExpectedToken,
    OutOfMem,
} || std.mem.Allocator.Error;

pub const Expr = union(enum) {
    Binary: struct {
        left: *Expr,
        op: Token,
        right: *Expr,
    },
    Unary: struct {
        op: Token,
        right: *Expr,
    },
    Grouped: struct {
        expr: *Expr,
    },
    Literal: struct {
        number: ?f64,
        string: ?[]const u8,
        boolean: ?bool,
        nul: ?void,
    },
    Variable: struct {
        name: Token,
    },
    Call: struct {
        callee: *Expr,
        paren: Token,
        args: []?*Expr,
    },
    FieldAccess: struct {
        target: *Expr,
        field: Token,
    },
    ArrayAccess: struct {
        array: *Expr,
        idx: *Expr,
    },
    Assignment: struct {
        name: Token,
        value: *Expr,
    },
    FieldAssignment: struct {
        target: *Expr,
        field: Token,
        value: *Expr,
    },

    pub fn deinit(self: *Expr, alloc: std.mem.Allocator) void {
        switch (self.*) {
            .Binary => |b| {
                b.left.deinit(alloc);
                b.right.deinit(alloc);
                alloc.destroy(b.left);
                alloc.destroy(b.right);
            },
            .Unary => |u| {
                u.right.deinit(alloc);
                alloc.destroy(u.right);
            },
            .Grouping => |g| {
                g.expr.deinit(alloc);
                alloc.destroy(g.expr);
            },
            .Call => |c| {
                for (c.args) |arg| {
                    if (arg) |a| {
                        a.deinit(alloc);
                        alloc.destroy(a);
                    }
                }
                alloc.free(c.args);
            },
            .Assignment => |a| {
                a.value.deinit(alloc);
                alloc.destroy(a.value);
            },
            .FieldAssignment => |f| {
                f.target.deinit(alloc);
                f.value.deinit(alloc);
                alloc.destroy(f.target);
                alloc.destroy(f.value);
            },
            else => {},
        }
    }
};

pub const Stmt = union(enum) {
    Expression: struct {
        expr: *Expr,
    },
    Var: struct {
        name: Token,
        type_an: ?Token, // type annotation
        init: ?*Expr,
    },
    Block: struct {
        stmts: []?*Stmt,
    },
    If: struct {
        condition: *Expr,
        then_brnch: *Stmt,
        else_brnch: ?*Stmt,
    },
    While: struct {
        condition: *Expr,
        body: *Stmt,
    },
    Function: struct {
        name: Token,
        params: []Param,
        return_type: ?Token,
        body: []?*Stmt,
    },
    Return: struct {
        keyword: Token,
        value: ?*Expr,
    },
    Struct: struct {
        name: Token,
        fields: []StructField,
    },
    Enum: struct {
        name: Token,
        variants: []EnumVariant,
    },
    Use: struct {
        path: []Token,
        alias: ?Token,
    },

    pub fn deinit(self: *Stmt, alloc: std.mem.Allocator) void {
        switch (self.*) {
            .Expression => |e| {
                e.expr.deinit(alloc);
                alloc.destroy(e.expr);
            },
            .Var => |l| {
                if (l.init) |i| {
                    i.deinit(alloc);
                    alloc.destroy(i);
                }
            },
            .Block => |b| {
                for (b.stmts) |stmt| {
                    if (stmt) |s| {
                        s.deinit(alloc);
                        alloc.destroy(s);
                    }
                }
                alloc.free(b.stmts);
            },
            .If => |i| {
                i.condition.deinit(alloc);
                i.then_brnch.deinit(alloc);
                if (i.else_brnch) |e_br| {
                    e_br.deinit(alloc);
                    alloc.destroy(e_br());
                }
                alloc.destroy(i.condition);
                alloc.destroy(i.then_brnch);
            },
            .Function => |f| {
                for (f.body) |stmt| {
                    if (stmt) |s| {
                        s.deinit(alloc);
                        alloc.destroy(s);
                    }
                }
                alloc.free(f.body);
                alloc.free(f.params);
            },
            else => {},
        }
    }
};

pub const Param = struct {
    name: Token,
    type_tok: Token,
};

pub const StructField = struct {
    name: Token,
    type_tok: Token,
    is_pub: bool,
};

pub const EnumVariant = struct {
    name: Token,
    fields: ?[]StructField,
};

pub const Parser = struct {
    tokens: []const Token,
    curr: usize = 0,
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator, tokens: []const Token) Parser {
        return .{ .tokens = tokens, .alloc = alloc };
    }

    pub fn parse(self: *Parser) ![]?*Stmt {
        var stmts = std.ArrayList(?*Stmt).init(self.alloc);
        defer stmts.deinit();

        while (!self.at_end()) {
            const stmt = try self.declaration();
            try stmts.append(stmt);
        }

        return stmts.toOwnedSlice();
    }

    fn declaration(self: *Parser) ParError!?*Stmt {
        if (self.match(&.{.Func})) {
            return try self.function("function");
        } else if (self.match(&.{.Var})) {
            return try self.var_declaration();
        } else if (self.match(&.{.Struct})) {
            return try self.struct_declaration();
        } else if (self.match(&.{.Enum})) {
            return try self.enum_declaration();
        } else if (self.match(&.{.Use})) {
            return try self.use_declaration();
        }

        return try self.statement();
    }

    fn statement(self: *Parser) ParError!?*Stmt {
        if (self.match(.If)) {
            return try self.if_statement();
        }

        if (self.match(.While)) {
            return try self.while_statement();
        }

        if (self.match(.Return)) {
            return try self.return_statement();
        }

        if (self.match(.LBrace)) {
            const stmt = try self.alloc.create(Stmt);
            stmt.* = .{ .Block = .{ .stmts = try self.block() } };
            return stmt;
        }

        return try self.expression_stmt();
    }

    fn if_statement(self: *Parser) ParError!*Stmt {
        try self.consume(.LParen, "expected '(' after 'if'.");
        const condition = try self.expression();
        try self.consume(.RParen, "expected ')' after condition.");

        const then_branch = (try self.statement()) orelse return error.InvalidStatement;
        var else_branch: ?*Stmt = null;

        if (self.match(.Else)) {
            else_branch = (try self.statement()) orelse return error.InvalidStatement;
        }

        const stmt = try self.alloc.create(Stmt);
        stmt.* = .{ .If = .{
            .condition = condition,
            .then_brnch = then_branch,
            .else_brnch = else_branch,
        } };

        return stmt;
    }

    fn while_statement(self: *Parser) ParError!*Stmt {
        try self.consume(.LParen, "expected '(' after 'while'.");
        const condition = try self.expression();
        try self.consume(.RParen, "expected ')' after condition.");

        const body = (try self.statement()) orelse return error.InvalidStatement;
        const stmt = try self.alloc.create(Stmt);
        stmt.* = .{
            .While = .{ .condition = condition, .body = body },
        };
        return stmt;
    }

    fn return_statement(self: *Parser) ParError!*Stmt {
        const kw = self.prev();
        var val: ?*Expr = null;

        if (!self.check(.Semi)) {
            val = try self.expression();
        }

        try self.consume(.Semi, "expected ';' after return value.");

        const stmt = try self.alloc.create(Stmt);
        stmt.* = .{ .Return = .{
            .keyword = kw,
            .value = val,
        } };

        return stmt;
    }

    fn block(self: *Parser) ParError!*Stmt {
        var stmts = std.ArrayList(?*Stmt).init(self.alloc);
        defer stmts.deinit();

        while (!self.check(.RBrace) and !self.at_end()) {
            try stmts.append(try self.declaration());
        }

        try self.consume(.RBrace, "expected '}' after block.");
        return stmts.toOwnedSlice();
    }

    fn var_declaration(self: *Parser) ParError!*Stmt {
        const name = try self.consume(.Identifier, "expected variable name.");

        var type_an: ?Token = null;
        if (self.match(&.{.Colon})) {
            type_an = try self.consume(.Identifier, "expected type after ':'");
        }

        var initi: ?*Expr = null;
        if (self.match(&.{.Equal})) {
            initi = try self.expression();
        }

        _ = try self.consume(.Semi, "expected ';' after variable declaration.");

        const stmt = try self.alloc.create(Stmt);
        stmt.* = .{ .Var = .{
            .name = name,
            .type_an = type_an,
            .init = initi,
        } };
        return stmt;
    }

    fn expression_stmt(self: *Parser) ParError!*Stmt {
        const expr = try self.expression();
        try self.consume(.Semi, "expected ';' after expression.");

        const stmt = try self.alloc.create(Stmt);
        stmt.* = .{ .Expression = .{ .expr = expr } };
        return stmt;
    }

    fn struct_declaration(self: *Parser) ParError!*Stmt {
        const name = try self.consume(.Identifier, "expected struct name.");

        _ = try self.consume(.LBrace, "expected '{' before struct body.");

        var fields = std.ArrayList(StructField).init(self.alloc);
        defer fields.deinit();

        while (!self.check(.RBrace) and !self.at_end()) {
            const is_pub = self.match(.Pub);
            const field_name = try self.consume(.Identifier, "expected field name.");
            try self.consume(.Colon, "expected ':' after field name.");
            const field_type = try self.consume(.Identifier, "expected field type.");

            try fields.append(.{
                .name = field_name,
                .type_tok = field_type,
                .is_pub = is_pub,
            });

            if (!self.check(.RBrace)) {
                try self.consume(.Comma, "expected ',' after field.");
            }
        }

        try self.consume(.RBrace, "expected '}' after struct fields.");

        const stmt = try self.alloc.create(Stmt);
        stmt.* = .{ .Struct = .{ .name = name, .fields = try fields.toOwnedSlice() } };

        return stmt;
    }

    fn enum_declaration(self: *Parser) ParError!*Stmt {
        const name = try self.consume(.Identifier, "expected enum name.");
        _ = try self.consume(.LBrace, "expected '{' before enum variants.");

        var variants = std.ArrayList(EnumVariant).init(self.alloc);
        defer variants.deinit();

        while (!self.check(.RBrace) and !self.at_end()) {
            const var_name = try self.consume(.Identifier, "expected variant name.");
            var fields: ?[]StructField = null;

            if (self.match(.LBrace)) {
                var var_fields = std.ArrayList(StructField).init(self.alloc);
                defer var_fields.deinit();

                while (!self.check(.RBrace) and !self.at_end()) {
                    const field_name = try self.consume(.Identifier, "expected field name.");
                    try self.consume(.Colon, "expected ':' after field name.");
                    const field_type = try self.consume(.Identifier, "expected field type.");

                    try var_fields.append(.{
                        .name = field_name,
                        .type_tok = field_type,
                        .is_pub = false,
                    });

                    if (!self.check(.RBrace)) {
                        try self.consume(.Comma, "expected ',' after field.");
                    }
                }

                try self.consume(.RBrace, "expected '}' after enum variant fields.");
                fields = try var_fields.toOwnedSlice();
            }

            try variants.append(.{
                .name = var_name,
                .fields = fields,
            });

            if (!self.check(.RBrace)) {
                try self.consume(.Comma, "expected ',' after enum variant.");
            }
        }

        try self.consume(.RBrace, "expected '}' after enum variants.");

        const stmt = try self.alloc.create(Stmt);
        stmt.* = .{
            .Enum = .{
                .name = name,
                .variants = try variants.toOwnedSlice(),
            },
        };
        return stmt;
    }

    fn use_declaration(self: *Parser) ParError!*Stmt {
        var path = std.ArrayList(Token).init(self.alloc);
        defer path.deinit();

        try path.append(try self.consume(.Identifier, "expected module name when using 'use'"));

        while (self.match(&.{.Dot})) {
            try path.append(try self.consume(.Identifier, "expected identifier after '.' in 'use'"));
        }

        var alias: ?Token = null;
        if (self.match(.As)) {
            alias = try self.consume(.Identifier, "expected alias name after 'as' in 'use'");
        }

        try self.consume(.Semi, "expected ';' after use declaration.");

        const stmt = try self.alloc.create(Stmt);
        stmt.* = .{
            .Use = .{
                .path = try path.toOwnedSlice(),
                .alias = alias,
            },
        };

        return stmt;
    }

    fn expression(self: *Parser) ParError!*Expr {
        return try self.assignment();
    }

    fn assignment(self: *Parser) ParError!*Expr {
        var expr = try self._or();

        if (self.match(.Equal)) {
            _ = self.prev(); // const equals
            const val = try self.assignment();

            switch (expr.*) {
                .Variable => |v| {
                    const new_expr = try self.alloc.create(Expr);
                    new_expr.* = .{
                        .Assignment = .{
                            .name = v.name,
                            .value = val,
                        },
                    };
                    expr.deinit(self.alloc);
                    self.alloc.destroy(expr);
                    return new_expr;
                },
                .FieldAccess => |f| {
                    const new_expr = try self.alloc.create(Expr);
                    new_expr.* = .{
                        .FieldAssignment = .{
                            .target = f.target,
                            .field = f.field,
                            .value = f.value,
                        },
                    };

                    self.alloc.destroy(expr);
                    return new_expr;
                },
                else => return error.InvalidAssignment,
            }
        }

        return expr;
    }

    fn _or(self: *Parser) ParError!*Expr {
        var expr = try self._and();

        while (self.match(&.{.Or})) {
            const op = self.prev();
            const right = try self._and();

            const next_expr = try self.alloc.create(Expr);
            next_expr.* = .{ .Binary = .{
                .left = expr,
                .op = op,
                .right = right,
            } };
            expr = next_expr;
        }

        return expr;
    }

    fn _and(self: *Parser) ParError!*Expr {
        var expr = try self.eq();

        while (self.match(&.{.And})) {
            const op = self.prev();
            const right = try self.eq();

            const new_expr = try self.alloc.create(Expr);
            new_expr.* = .{
                .Binary = .{ .left = expr, .op = op, .right = right },
            };

            expr = new_expr;
        }

        return expr;
    }

    fn eq(self: *Parser) ParError!*Expr {
        var expr = try self.cmp();

        while (self.match(&.{.BEqual}) or self.match(&.{.EEqual})) {
            const op = self.prev();
            const right = try self.cmp();

            const new_expr = try self.alloc.create(Expr);
            new_expr.* = .{
                .Binary = .{
                    .left = expr,
                    .op = op,
                    .right = right,
                },
            };

            expr = new_expr;
        }

        return expr;
    }

    fn cmp(self: *Parser) ParError!*Expr {
        var expr = try self.term();

        while (self.match(&.{.Greater}) or self.match(&.{.GEqual}) or self.match(&.{.Less}) or self.match(&.{.LEqual})) {
            const op = self.prev();
            const right = try self.term();

            const new_expr = try self.alloc.create(Expr);
            new_expr.* = .{
                .Binary = .{ .left = expr, .op = op, .right = right },
            };

            expr = new_expr;
        }

        return expr;
    }

    fn term(self: *Parser) ParError!*Expr {
        var expr = try self.factor();

        while (self.match(&.{.Plus}) or self.match(&.{.Minus})) {
            const op = self.prev();
            const right = try self.factor();

            const new_expr = try self.alloc.create(Expr);
            new_expr.* = .{
                .Binary = .{ .left = expr, .op = op, .right = right },
            };

            expr = new_expr;
        }

        return expr;
    }

    fn factor(self: *Parser) ParError!*Expr {
        var expr = try self.unary();

        while (self.match(&.{.Star}) or self.match(&.{.Slash})) {
            const op = self.prev();
            const right: *Expr = self.unary();

            const new_expr = try self.alloc.create(Expr);
            new_expr.* = .{
                .Binary = .{
                    .left = expr,
                    .op = op,
                    .right = right,
                },
            };

            expr = new_expr;
        }

        return expr;
    }

    fn unary(self: *Parser) ParError!*Expr {
        if (self.match(&.{.Bang}) or self.match(&.{.Minus})) {
            const op = self.prev();
            const right = try self.unary();

            const expr = try self.alloc.create(Expr);
            expr.* = .{ .Unary = .{
                .op = op,
                .right = right,
            } };

            return expr;
        }

        return try self.call();
    }

    fn call(self: *Parser) ParError!*Expr {
        var expr = try self.primary();

        while (true) {
            if (self.match(&.{.LParen})) {
                expr = try self.end_call(expr);
            } else if (self.match(&.{.Dot})) {
                expr = try self.field_access(expr);
            } else if (self.match(&.{.LBracket})) {
                const idx = try self.expression();
                _ = try self.consume(.RBracket, "Expected ']' after array index");
                const new_expr = try self.alloc.create(Expr);
                new_expr.* = .{ .ArrayAccess = .{
                    .array = expr,
                    .idx = idx,
                } };
                expr = new_expr;
            } else {
                break;
            }
        }

        return expr;
    }

    fn end_call(self: *Parser, callee: *Expr) ParError!*Expr {
        var args = std.ArrayList(?*Expr).init(self.alloc);
        defer args.deinit();

        if (!self.check(.RParen)) {
            while (true) {
                try args.append(try self.expression());
                if (!self.match(&.{.Comma})) break;
            }
        }

        const paren = try self.consume(.RParen, "expected ')' after arguments.");

        const expr = try self.alloc.create(Expr);
        expr.* = .{ .Call = .{
            .callee = callee,
            .paren = paren,
            .args = try args.toOwnedSlice(),
        } };

        return expr;
    }

    fn function(self: *Parser, kind: []const u8) ParError!*Stmt {
        const name = try self.consume(.Identifier, try std.fmt.allocPrint(self.alloc, "expected {s} name.", .{kind}));

        _ = try self.consume(.LParen, try std.fmt.allocPrint(self.alloc, "expected '(' after {s} name.", .{kind}));
        var params = std.ArrayList(Param).init(self.alloc);
        defer params.deinit();

        if (!self.check(.RParen)) {
            while (true) {
                const param_name = try self.consume(.Identifier, "expected parameter name.");
                _ = try self.consume(.Colon, "expected ':' after parameter name.");
                const param_type = try self.consume(.Identifier, "expected parameter type.");

                try params.append(.{
                    .name = param_name,
                    .type_tok = param_type,
                });

                if (!self.match(&.{.Comma})) break;
            }
        }

        _ = try self.consume(.RParen, "expected ')' after parameters.");

        var return_type: ?Token = null;
        if (self.match(&.{.Arrow})) {
            return_type = try self.consume(.Identifier, "expected return type after '->'.");
        }

        _ = try self.consume(.LBrace, "expected '{' before function body.");
        var body = std.ArrayList(?*Stmt).init(self.alloc);
        defer body.deinit();

        while (!self.check(.RBrace) and !self.at_end()) {
            try body.append(try self.declaration());
        }

        _ = try self.consume(.RBrace, "expected '}' after function body.");

        const stmt = try self.alloc.create(Stmt);
        stmt.* = .{
            .Function = .{
                .name = name,
                .params = try params.toOwnedSlice(),
                .return_type = return_type,
                .body = try body.toOwnedSlice(),
            },
        };

        return stmt;
    }

    fn field_access(self: *Parser, expr: *Expr) ParError!*Expr {
        const name = try self.consume(.Identifier, "expected field name after '.'");
        const new_expr = try self.alloc.create(Expr);
        new_expr.* = .{ .FieldAccess = .{
            .target = expr,
            .field = name,
        } };

        return new_expr;
    }

    fn primary(self: *Parser) ParError!*Expr {
        const expr = try self.alloc.create(Expr);

        if (self.match(&.{.False})) {
            expr.* = .{ .Literal = .{ .boolean = false, .number = null, .string = null, .nul = null } };
            return expr;
        }

        if (self.match(&.{.True})) {
            expr.* = .{ .Literal = .{ .boolean = true, .number = null, .string = null, .nul = null } };
            return expr;
        }

        if (self.match(&.{.Null})) {
            expr.* = .{ .Literal = .{ .nul = {}, .boolean = null, .number = null, .string = null } };
            return expr;
        }

        if (self.match(&.{.Number})) {
            const val = self.prev().literal.number;
            expr.* = .{ .Literal = .{ .number = val, .nul = null, .string = null, .boolean = null } };
            return expr;
        }

        if (self.match(&.{.String})) {
            const val = self.prev().literal.string;
            expr.* = .{ .Literal = .{ .string = val, .number = null, .nul = null, .boolean = null } };
            return expr;
        }

        if (self.match(&.{.Identifier})) {
            expr.* = .{ .Variable = .{ .name = self.prev() } };
            return expr;
        }

        if (self.match(&.{.LParen})) {
            const inner_expr = try self.expression();
            _ = try self.consume(.RParen, "expected ')' after expression.");
            expr.* = .{ .Grouped = .{ .expr = inner_expr } };
            return expr;
        }

        self.alloc.destroy(expr);
        return error.InvalidExpression;
    }

    fn match(self: *Parser, types: []const TokenType) bool {
        for (types) |t| {
            if (self.check(t)) {
                const token = self.advance();
                _ = token;
                return true;
            }
        }
        return false;
    }

    fn check(self: *Parser, t: TokenType) bool {
        if (self.at_end()) return false;
        return self.peek().type == t;
    }

    fn advance(self: *Parser) Token {
        if (!self.at_end()) self.curr += 1;
        return self.prev();
    }

    fn consume(self: *Parser, t: TokenType, msg: []const u8) !Token {
        if (self.check(t)) return self.advance();
        std.debug.print("%{s}\n", .{msg});
        return error.ExpectedToken;
    }

    fn at_end(self: *Parser) bool {
        return self.peek().type == .EOF;
    }

    fn peek(self: *Parser) Token {
        return self.tokens[self.curr];
    }

    fn prev(self: *Parser) Token {
        return self.tokens[self.curr - 1];
    }
};
