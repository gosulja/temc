const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Literal = @import("token.zig").Literal;

pub const LexError = error{
    IllegalCharacter,
    UnterminatedString,
    InvalidNumber,
    OutOfMem,
};

const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "func", .Func },
    .{ "return", .Return },
    .{ "var", .Var },
    .{ "const", .Const },
    .{ "if", .If },
    .{ "else", .Else },
    .{ "while", .While },
    .{ "for", .For },
    .{ "break", .Break },
    .{ "continue", .Continue },
    .{ "struct", .Struct },
    .{ "enum", .Enum },
    .{ "type", .Type },
    .{ "pub", .Pub },
    .{ "use", .Use },
    .{ "as", .As },
    .{ "switch", .Switch },
    .{ "true", .True },
    .{ "false", .False },
    .{ "null", .Null },
});

pub const Lexer = struct {
    src: []const u8,
    tokens: std.ArrayList(Token),
    start: usize,
    curr: usize,
    line: usize,
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator, src: []const u8) Lexer {
        return .{
            .src = src,
            .tokens = std.ArrayList(Token).init(alloc),
            .start = 0,
            .curr = 0,
            .line = 0,
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *Lexer) void {
        self.tokens.deinit();
    }

    pub fn scan_all(self: *Lexer) ![]Token {
        while (!self.at_end()) {
            self.start = self.curr;
            try self.scan();
        }

        try self.tokens.append(Token.init(
            .EOF,
            "",
            .{ .none = {} },
            self.line,
        ));

        return self.tokens.items;
    }

    fn scan(self: *Lexer) !void {
        const c = self.advance();
        switch (c) {
            '(' => try self.add_token(.LParen),
            ')' => try self.add_token(.RParen),
            '{' => try self.add_token(.LBrace),
            '}' => try self.add_token(.RBrace),
            '[' => try self.add_token(.LBracket),
            ']' => try self.add_token(.RBracket),
            ',' => try self.add_token(.Comma),
            '.' => try self.add_token(.Dot),
            '-' => {
                if (self.match('>')) {
                    try self.add_token(.Arrow);
                } else {
                    try self.add_token(.Minus);
                }
            },
            '+' => try self.add_token(.Plus),
            ';' => try self.add_token(.Semi),
            '*' => try self.add_token(.Star),
            ':' => try self.add_token(.Colon),
            '=' => {
                if (self.match('=')) {
                    try self.add_token(.EEqual);
                } else if (self.match('>')) {
                    try self.add_token(.FArrow);
                } else {
                    try self.add_token(.Equal);
                }
            },
            '!' => try self.add_token(if (self.match('=')) .BEqual else .Bang),
            '<' => try self.add_token(if (self.match('=')) .LEqual else .Less),
            '>' => try self.add_token(if (self.match('=')) .GEqual else .Greater),
            '/' => {
                if (self.match('/')) {
                    while (!self.at_end() and self.peek() != '\n') {
                        _ = self.advance();
                    }
                } else if (self.match('*')) {
                    try self.multiline_comment();
                } else {
                    try self.add_token(.Slash);
                }
            },

            ' ', '\r', '\t' => {},
            '\n' => self.line += 1,

            '"' => try self.string(),

            else => {
                if (self.is_digit(c)) {
                    try self.number();
                } else if (self.is_alpha(c)) {
                    try self.identifier();
                } else {
                    return error.IllegalCharacter;
                }
            },
        }
    }

    fn multiline_comment(self: *Lexer) !void {
        var nesting: usize = 1;
        while (nesting > 0 and !self.at_end()) {
            if (self.peek() == '/' and self.peek_next() == '*') {
                _ = self.advance();
                _ = self.advance();
                nesting += 1;
            } else if (self.peek() == '*' and self.peek_next() == '/') {
                _ = self.advance();
                _ = self.advance();
                nesting -= 1;
            } else {
                if (self.peek() == '\n') self.line += 1;
                _ = self.advance();
            }
        }
    }

    fn string(self: *Lexer) !void {
        while (!self.at_end() and self.peek() != '"') {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.at_end()) {
            return error.UnterminatedString;
        }

        _ = self.advance();

        // trim quutes
        const lit = self.src[self.start + 1 .. self.curr - 1];
        try self.add_token_lit(.String, .{ .string = lit });
    }

    fn number(self: *Lexer) !void {
        while (self.is_digit(self.peek())) {
            _ = self.advance();
        }

        // decimal
        if (self.peek() == '.' and self.is_digit(self.peek_next())) {
            _ = self.advance();

            while (self.is_digit(self.peek())) {
                _ = self.advance();
            }
        }

        const lit = std.fmt.parseFloat(f64, self.src[self.start..self.curr]) catch {
            return error.InvalidNumber;
        };

        try self.add_token_lit(.Number, .{ .number = lit });
    }

    fn identifier(self: *Lexer) !void {
        while (self.is_alpnum(self.peek())) {
            _ = self.advance();
        }

        const lit = self.src[self.start..self.curr];
        const typ = keywords.get(lit) orelse .Identifier;
        try self.add_token(typ);
    }

    fn add_token(self: *Lexer, typ: TokenType) !void {
        try self.add_token_lit(typ, .{ .none = {} });
    }

    fn add_token_lit(self: *Lexer, typ: TokenType, lit: Literal) !void {
        const txt = self.src[self.start..self.curr];
        try self.tokens.append(Token.init(typ, txt, lit, self.line));
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.at_end()) return false;
        if (self.src[self.curr] != expected) return false;

        self.curr += 1;
        return true;
    }

    fn peek(self: *Lexer) u8 {
        if (self.at_end()) return 0;
        return self.src[self.curr];
    }

    fn peek_next(self: *Lexer) u8 {
        if (self.curr + 1 >= self.src.len) return 0;
        return self.src[self.curr + 1];
    }

    fn advance(self: *Lexer) u8 {
        const c = self.src[self.curr];
        self.curr += 1;
        return c;
    }

    fn at_end(self: *Lexer) bool {
        return self.curr >= self.src.len;
    }

    fn is_alpha(_: *Lexer, c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn is_digit(_: *Lexer, c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn is_alpnum(self: *Lexer, c: u8) bool {
        return self.is_alpha(c) or self.is_digit(c);
    }
};
