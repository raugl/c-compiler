const std = @import("std");
const util = @import("util.zig");
const op = @import("operator.zig");
const kw = @import("keyword.zig");
const tk = @import("token.zig");

const Token = tk.Token;
const testing = std.testing;

const StateMachine = union(enum) {
    new_token,
    starts_alphabetic,
    starts_bin,
    starts_digit,
    starts_hex,
    starts_octal,

    int_suffix: struct {
        is_unsigned: bool = false,
        width: enum { int, long, long_long, size } = .int,
    },
    float_fractional,
    float_exponent,
    float_suffix: enum { float, double, long },

    literal_bool,
    literal_char: struct {
        len: u32 = 0,
        is_escaped: bool = false,
    },
    literal_str: struct {
        is_escaped: bool = false,
    },

    block_comment,
    identifier,
    keyword,
    line_comment,
    operator,
    preproc,
};

pub const TokenIterator = struct {
    const Self = @This();

    source: []const u8,
    idx: u32,
    char: u21,
    char_len: u3,
    token_start_idx: u32 = 0,
    state: StateMachine = .new_token,

    pub fn init(src: []const u8) !Self {
        const len = try std.unicode.utf8CodepointSequenceLength(src[0]);
        const char = try std.unicode.utf8Decode(src[0..len]);
        return Self{
            .source = src,
            .idx = 0,
            .char = char,
            .char_len = len,
        };
    }

    pub fn next(self: *Self) !?tk.Token {
        try self.consumeWhitespace();
        self.token_start_idx = self.idx;
        self.state = .new_token;

        while (self.idx < self.source.len) {
            switch (self.state) {
                .new_token => {
                    if (self.char == '0') switch (try self.peekNextChar()) {
                        'x', 'X' => {
                            self.state = .starts_hex;
                            try self.consumeChar();
                            try self.consumeChar();
                        },
                        'b', 'B' => {
                            self.state = .starts_bin;
                            try self.consumeChar();
                            try self.consumeChar();
                        },
                        '0'...'9' => {
                            self.state = .starts_octal;
                            try self.consumeChar();
                        },
                        else => {
                            return error.InvalidTokenStart;
                        },
                    } else if (util.isNonZero(self.char)) {
                        self.state = .starts_digit;
                        try self.consumeChar();
                    } else if (util.isAlphabetic(self.char) or self.char == '_') {
                        self.state = .starts_alphabetic;
                        try self.consumeChar();
                    } else if (util.isSymbol(self.char)) {
                        switch (self.char) {
                            '#' => {
                                self.state = .preproc;
                                try self.consumeChar();
                            },
                            '"' => {
                                self.state = StateMachine{ .literal_str = .{} };
                                try self.consumeChar();
                            },
                            '\'' => {
                                self.state = StateMachine{ .literal_char = .{} };
                                try self.consumeChar();
                            },
                            '/' => switch (try self.peekNextChar()) {
                                '/' => {
                                    try self.consumeChar();
                                    try self.consumeChar();
                                    self.state = .line_comment;
                                },
                                '*' => {
                                    try self.consumeChar();
                                    try self.consumeChar();
                                    self.state = .block_comment;
                                },
                                else => {
                                    self.state = .operator;
                                    return self.finishToken();
                                },
                            },
                            else => {
                                self.state = .operator;
                                return self.finishToken();
                            },
                        }
                    } else {
                        return error.InvalidTokenStart;
                    }
                },
                .starts_digit => {
                    if (util.isDigit(self.char) or self.char == '\'') {
                        try self.consumeChar();
                    } else if (self.char == '.') {
                        self.state = .float_fractional;
                        try self.consumeChar();
                    } else {
                        self.state = StateMachine{ .int_suffix = .{} };
                    }
                },
                .starts_hex => {
                    if (util.isHex(self.char) or self.char == '\'') {
                        try self.consumeChar();
                    } else {
                        self.state = StateMachine{ .int_suffix = .{} };
                    }
                },
                .starts_bin => {
                    if (util.isBinary(self.char) or self.char == '\'') {
                        try self.consumeChar();
                    } else {
                        self.state = StateMachine{ .int_suffix = .{} };
                    }
                },
                .starts_octal => {
                    if (util.isOctal(self.char) or self.char == '\'') {
                        try self.consumeChar();
                    } else {
                        self.state = StateMachine{ .int_suffix = .{} };
                    }
                },
                .starts_alphabetic => {
                    if (util.isAlphabetic(self.char) or self.char == '_') {
                        try self.consumeChar();
                    } else if (util.isDigit(self.char)) {
                        self.state = .identifier;
                        try self.consumeChar();
                    } else {
                        if (util.isLiteralBool(self.tokenStr())) {
                            self.state = .literal_bool;
                            return self.finishToken();
                        } else if (util.isKeyword(self.tokenStr())) {
                            self.state = .keyword;
                            return self.finishToken();
                        } else {
                            self.state = .identifier;
                            return self.finishToken();
                        }
                    }
                },
                .identifier => {
                    if (util.isAlphanumeric(self.char) or self.char == '_') {
                        try self.consumeChar();
                    } else return self.finishToken();
                },
                .int_suffix => |*state| {
                    const State = @TypeOf(state.*);
                    var new_state = State{};
                    switch (self.char) {
                        'z', 'Z' => new_state.width = .size,
                        'u', 'U' => new_state.is_unsigned = true,
                        'l' => new_state.width = if (try self.peekNextChar() != 'l') .long else .long_long,
                        'L' => new_state.width = if (try self.peekNextChar() != 'L') .long else .long_long,
                        else => return self.finishToken(),
                    }

                    if (state.is_unsigned and new_state.is_unsigned) return error.InvalidIntSuffix;
                    if (state.width != .int and new_state.width != .int) return error.InvalidIntSuffix;

                    try self.consumeChar();
                    if (new_state.width == .long_long and state.width != .long_long) try self.consumeChar();
                    if (new_state.is_unsigned) state.is_unsigned = new_state.is_unsigned;
                    if (new_state.width != .int) state.width = new_state.width;
                },
                // TODO: This is incomplete/wrong, use this to fix it:
                // https://en.cppreference.com/w/cpp/language/floating_literal
                // .literal_float => switch (self.state.literal_float) {
                //     .fractional => {
                //         if (ch.isDigit(self.char)) {
                //             try self.consumeChar();
                //         } else switch (self.char) {
                //             'l', 'L' => {
                //                 self.state = StateMachine{ .literal_float = .long };
                //                 try self.consumeChar();
                //             },
                //             'f', 'F' => {
                //                 self.state = StateMachine{ .literal_float = .float };
                //                 try self.consumeChar();
                //             },
                //             'e', 'E' => {
                //                 self.state = StateMachine{ .literal_float = .exponent };
                //                 try self.consumeChar();
                //             },
                //             else => self.state = StateMachine{ .literal_float = .double },
                //         }
                //     },
                //     .exponent => {
                //         // TODO:
                //         unreachable;
                //     },
                //     else => return self.finishToken(),
                // },
                // FIXME: This just takes slice into the source code. Instead it
                // should allocate its own memory and remove special characters.
                .literal_str => |*state| {
                    if (self.char == '\\') {
                        state.is_escaped = true;
                    } else if (self.char == '"' and !state.is_escaped) {
                        try self.consumeChar();
                        return self.finishToken();
                    } else {
                        state.is_escaped = false;
                        try self.consumeChar();
                    }
                },
                .literal_char => |*state| {
                    switch (self.char) {
                        '\\' => state.is_escaped = true,
                        '\'' => if (!state.is_escaped) {
                            try self.consumeChar();
                            return self.finishToken();
                        },
                        else => {},
                    }
                    state.is_escaped = false;
                    try self.consumeChar();
                },
                .preproc => {
                    // TODO: Refine this by wrapping it in a more precise token
                    if (util.isLower(self.char)) {
                        try self.consumeChar();
                    } else {
                        self.state = .preproc;
                        return self.finishToken();
                    }
                },
                .line_comment => {
                    while (self.char != '\n' and self.char != '\r') {
                        try self.consumeChar();
                    }
                    return self.finishToken();
                },
                .block_comment => {
                    while (self.char != '*' or try self.peekNextChar() != '/') {
                        try self.consumeChar();
                    }
                    try self.consumeChar();
                    try self.consumeChar();
                    return self.finishToken();
                },
                .literal_bool, .keyword, .operator => {},
                else => {},
            }
        }

        return null;
    }

    fn tokenStr(self: Self) []const u8 {
        return self.source[self.token_start_idx..self.idx];
    }

    fn consumeWhitespace(self: *Self) !void {
        var source = self.source[self.idx..];
        if (self.idx == self.source.len or !std.ascii.isWhitespace(source[0])) {
            return;
        }

        if (std.mem.indexOfNone(u8, source, &std.ascii.whitespace)) |len| {
            self.idx += @intCast(len);

            source = self.source[self.idx..];
            self.char_len = try std.unicode.utf8CodepointSequenceLength(source[0]);
            self.char = try std.unicode.utf8Decode(source[0..self.char_len]);
        } else {
            self.idx += @intCast(source.len);
        }
    }

    fn consumeChar(self: *Self) !void {
        self.idx += self.char_len;
        const source = self.source[self.idx..];
        self.char_len = try std.unicode.utf8CodepointSequenceLength(source[0]);
        self.char = try std.unicode.utf8Decode(source[0..self.char_len]);
    }

    // PERF: Maybe cache this
    fn peekNextChar(self: Self) !u21 {
        const idx = self.idx + self.char_len;
        const source = self.source[idx..];
        const char_len = try std.unicode.utf8CodepointSequenceLength(source[0]);
        return try std.unicode.utf8Decode(source[0..char_len]);
    }

    fn finishToken(self: *Self) !?Token {
        switch (self.state) {
            .int_suffix => |state| {
                const bit_width: u8 = switch (state.width) {
                    .int, .long => 32,
                    .long_long, .size => 64, // TODO: `.size` should be dependent
                    // on the target platform architecture, but as I currently
                    // only support amd64_linux, I can hard-code this value
                };

                return Token{ .literal_int = .{
                    .bit_width = bit_width,
                    .signed = !state.is_unsigned,
                    .value = try util.parseInt(self.tokenStr()),
                } };
            },
            // .literal_float => |state| {
            //     const bit_width: u8 = switch (state) {
            //         .float => 32,
            //         .double => 64,
            //         .long => 128,
            //         else => unreachable,
            //     };
            //     const value = try std.fmt.parseFloat(f128, self.tokenStr());
            //
            //     return Token{ .literal_float = .{
            //         .bit_width = bit_width,
            //         .value = value,
            //     } };
            // },
            .literal_bool => {
                if (std.mem.eql(u8, self.tokenStr(), "true")) {
                    return Token{ .literal_bool = true };
                } else {
                    return Token{ .literal_bool = false };
                }
            },
            .literal_char => {
                var char: u8 = undefined;
                var str = self.tokenStr();
                str = str[1..(str.len - 1)]; // Trim quotes (`'`)

                if (std.mem.eql(u8, str, "")) {
                    return error.EmptyCharLiteral;
                } else if (std.mem.eql(u8, str, "\\")) {
                    return error.UnclosedSingleQuote;
                } else if (str[0] == '\\') {
                    char = switch (str[1]) {
                        '0' => 0, // \0: Null Character - ASCII value 0 (this can also be followed by additional digits to specify octal values)
                        'a' => 7, // \a: Bell (Alert) - ASCII value 7
                        'b' => 8, // \b: Backspace - ASCII value 8
                        't' => 9, // \t: Horizontal Tab - ASCII value 9
                        'n' => 10, // \n: Newline (Line Feed) - ASCII value 10
                        'v' => 11, // \v: Vertical Tab - ASCII value 11
                        'f' => 12, // \f: Form Feed - ASCII value 12
                        'r' => 13, // \r: Carriage Return - ASCII value 13
                        '\'' => 39, // \': Single Quote - ASCII value 39
                        '?' => 63, // \?: Question Mark - ASCII value 63 (used to avoid trigraphs)
                        '\\' => 92, // \\: Backslash - ASCII value 92
                        else => unreachable,
                    };
                } else {
                    char = str[0];
                }

                return Token{ .literal_int = .{
                    .bit_width = 8,
                    .signed = false,
                    .value = char,
                } };
            },
            .literal_str => {
                var str = self.tokenStr();
                str = str[1..(str.len - 1)]; // Trim quotes (`"`)
                // FIXME: Allocate a new byte array and copy the string to it with all the special characters removed
                return Token{ .literal_str = str };
            },
            .identifier => {
                return Token{ .identifier = self.tokenStr() };
            },
            .keyword => {
                const res = kw.parseKeyword(self.tokenStr()) orelse return error.InvalidKeyword;

                if (self.idx < self.source.len) {
                    const source = self.source[self.idx..];
                    self.char_len = try std.unicode.utf8CodepointSequenceLength(source[0]);
                    self.char = try std.unicode.utf8Decode(source[0..self.char_len]);
                }
                return Token{ .keyword = res.kw };
            },
            .operator => {
                const res = op.parseOperator(self.source[self.idx..]) orelse return error.InvalidOperator;
                self.idx += res.len;

                if (self.idx < self.source.len) {
                    const source = self.source[self.idx..];
                    self.char_len = try std.unicode.utf8CodepointSequenceLength(source[0]);
                    self.char = try std.unicode.utf8Decode(source[0..self.char_len]);
                }
                return Token{ .operator = res.op };
            },
            .preproc => {
                // TODO:
                return Token{ .preproc = .include };
            },
            .line_comment, .block_comment => {
                return Token{ .comment = self.tokenStr() };
            },
            .new_token, .starts_digit, .starts_alphabetic => {
                return error.IncompleteToken;
            },
            else => return Token{ .comment = "unimplemented" },
        }
    }
};

test "TokenIterator" {
    const TestCase = struct {
        expected_token: Token,

        fn init(token: Token) @This() {
            return .{ .expected_token = token };
        }
    };

    // FIXME: This test should be failing
    const source =
        \\#include "stdio.h"
        \\
        \\int main(int argc, char** argv) {
        \\  printf("hello, %s", "world");
        \\}
    ;
    const makeCase = TestCase.init;
    const cases = [_]TestCase{
        makeCase(Token{ .identifier = "#include" }),
        makeCase(Token{ .identifier = "\"stdio.h\"" }),
        makeCase(Token{ .identifier = "int" }),
        makeCase(Token{ .identifier = "main" }),
        makeCase(Token{ .identifier = "(" }),
        makeCase(Token{ .identifier = "int" }),
        makeCase(Token{ .identifier = "argc" }),
        makeCase(Token{ .identifier = "," }),
        makeCase(Token{ .identifier = "char" }),
        makeCase(Token{ .identifier = "*" }),
        makeCase(Token{ .identifier = "*" }),
        makeCase(Token{ .identifier = "argv" }),
        makeCase(Token{ .identifier = ")" }),
        makeCase(Token{ .identifier = "{" }),
        makeCase(Token{ .identifier = "printf" }),
        makeCase(Token{ .identifier = "(" }),
        makeCase(Token{ .identifier = "\"hello, %s\"" }),
        makeCase(Token{ .identifier = "," }),
        makeCase(Token{ .identifier = "\"world\"" }),
        makeCase(Token{ .identifier = ")" }),
        makeCase(Token{ .identifier = ";" }),
        makeCase(Token{ .identifier = "}" }),
    };

    var iter = try TokenIterator.init(source);
    var i = @as(usize, 0);
    while (try iter.next()) |token| {
        const case = cases[i];
        try std.testing.expectEqualStrings(case.expected_token.identifier, token.identifier);
        i += 1;
    }
}
