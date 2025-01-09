const std = @import("std");

pub const TokenType = enum {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Semi,
    Slash,
    Star,
    Colon,
    Arrow, // ->
    FArrow, // =>
    Bang,
    BEqual, // !=
    Equal,
    EEqual, // ==
    Greater,
    GEqual,
    Less,
    LEqual,
    Identifier,
    String,
    Number,
    Func,
    Return,
    Var,
    Const,
    If,
    Else,
    While,
    True,
    False,
    Null,
    For,
    Break,
    Continue,
    Struct,
    Enum,
    Type,
    Pub,
    Use,
    As,
    Switch,
    EOF,
};

pub const Literal = union(enum) {
    string: []const u8,
    number: f64,
    none: void,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    literal: Literal,
    line: usize,

    pub fn init(
        typ: TokenType,
        lexeme: []const u8,
        literal: Literal,
        line: usize,
    ) Token {
        return .{
            .type = typ,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }
};
