const std = @import("std");
const util = @import("util.zig");
const tk = @import("token.zig");
const kw = @import("keyword.zig");
const op = @import("operator.zig");

const Token = tk.Token;
const ch = util.char;

pub const Lexer = struct {
    const Self = @This();

    idx: u32 = 0,
    token_start: u32 = 0,
    source: []const u8,
    first_on_line: bool = true, // TODO: for macros
    idx_stack: std.ArrayList(u32),

    /// Deinitialize with `deinit`.
    pub fn init(alloc: std.mem.Allocator, source: []const u8) Self {
        return Self{
            .source = source,
            .idx_stack = std.ArrayList(u32).init(alloc),
        };
    }

    pub fn deinit(self: *Self) void {
        self.idx_stack.deinit();
    }

    pub fn next(self: *Self) !?Token {
        optional(self.match(ch.whitespace));
        self.token_start = self.idx;

        // NOTE: The order matters
        const token =
            self.literalBool() orelse
            self.keyword() orelse
            self.identifier() orelse
            self.preproc() orelse
            try self.comment() orelse
            try self.literalStr() orelse
            try self.literalChar() orelse
            self.operator() orelse
            try self.literalNumeric();

        if (token == null) {
            const source = self.source[self.idx..];
            const no_more_chars = std.mem.indexOfNone(u8, source, &std.ascii.whitespace) == null;
            return if (no_more_chars) token else error.InvalidToken;
        }
        return token;
    }

    fn literalBool(self: *Self) ?Token {
        if (self.match("true")) {
            return Token{ .literal_bool = true };
        }
        if (self.match("false")) {
            return Token{ .literal_bool = false };
        }
        return null;
    }

    fn identifier(self: *Self) ?Token {
        if (self.match('_') or self.match(ch.alphabetic)) {
            while (self.match('_') or self.match(ch.alphaNumeric)) {}
            return Token{ .identifier = self.tokenStr() };
        }
        return null;
    }

    fn comment(self: *Self) !?Token {
        if (self.match("//")) {
            const source = self.source[self.idx..];
            const len = std.mem.indexOfAny(u8, source, "\r\n") orelse source.len;
            self.idx += @intCast(len);
            return Token{ .comment = self.tokenStr() };
        }
        if (self.match("/*")) {
            const source = self.source[self.idx..];
            const len = std.mem.indexOf(u8, source, "*/") orelse return error.UnmatchedBlockComment;
            self.idx += @intCast(len + 2);
            return Token{ .comment = self.tokenStr() };
        }
        return null;
    }

    fn keyword(self: *Self) ?Token {
        const source = self.source[self.idx..];
        const res = kw.parseKeyword(source) orelse return null;
        self.idx += res.len;
        return Token{ .keyword = res.kw };
    }

    fn operator(self: *Self) ?Token {
        const source = self.source[self.idx..];
        const res = op.parseOperator(source) orelse return null;
        self.idx += res.len;
        return Token{ .operator = res.op };
    }

    // TODO:
    fn preproc(self: *Self) ?Token {
        self.pushIdx();
        defer self.popIdx();

        if (self.first_on_line and self.match('#')) {
            optional(self.match(ch.whitespace));

            if (self.match("include")) {
                return Token{ .preproc = .include };
            } else if (self.match("pragma")) {
                return Token{ .preproc = .pragma };
            } else if (self.match("define")) {
                return Token{ .preproc = .define };
            } else if (self.match("undef")) {
                return Token{ .preproc = .undef };
            } else if (self.match("ifndef")) {
                return Token{ .preproc = .ifndef };
            } else if (self.match("ifdef")) {
                return Token{ .preproc = .ifdef };
            } else if (self.match("if")) {
                return Token{ .preproc = .if_ };
            } else if (self.match("else")) {
                return Token{ .preproc = .else_ };
            } else if (self.match("elif")) {
                return Token{ .preproc = .elif };
            } else if (self.match("elifdef")) {
                return Token{ .preproc = .elifdef };
            } else if (self.match("elifndef")) {
                return Token{ .preproc = .elifndef };
            } else if (self.expect("endif")) {
                return Token{ .preproc = .endif };
            }
        }
        return null;
    }

    // TODO:
    fn literalStr(self: *Self) !?Token {
        if (self.match('"')) {
            const source = self.source[self.idx..];
            const len = std.mem.indexOfScalar(u8, source, '"') orelse return error.UnmatchedDoubleQuote;
            self.idx += @intCast(len + 1);
            return Token{ .literal_str = self.tokenStr() };
        }
        return null;
    }

    // TODO:
    fn literalChar(self: *Self) !?Token {
        if (self.match("'")) {
            const source = self.source[self.idx..];
            const len = std.mem.indexOfScalar(u8, source, '\'') orelse return error.UnmatchedQuote;
            self.idx += @intCast(len + 1);
            return Token{ .literal_str = self.tokenStr() };
        }
        return null;
    }

    fn literalNumeric(self: *Self) !?Token {
        self.pushIdx();
        defer self.popIdx();

        // NOTE: The order matters
        if (try self.literalFloat(.dec)) |token| {
            return token;
        }
        if (self.match("0b") or self.match("0B")) {
            return self.expect(try self.literalInt(ch.binary));
        }
        if (self.match("0x") or self.match("0X")) {
            return self.expect(try self.literalFloat(.hex) orelse try self.literalInt(ch.hex));
        }
        if (self.peek('0')) {
            return self.expect(try self.literalInt(ch.octal));
        }
        if (self.peek(ch.nonZero)) {
            return self.literalInt(ch.decimal);
        }
        return null;
    }

    fn literalInt(self: *Self, digitFn: MatchFn) !?Token {
        self.pushIdx();
        defer self.popIdx();

        if (!self.expect(self.literalDigitSequence(digitFn))) return null;
        const value = try util.parseInt(self.tokenStr());
        const suffix = self.literalIntSuffix() orelse LiteralIntSuffixResult{};

        return Token{ .literal_int = .{
            .value = value,
            .width = suffix.width,
            .signed = suffix.signed,
        } };
    }

    const LiteralIntSuffixResult = struct {
        width: u8 = 32,
        signed: bool = true,
    };

    fn literalIntSuffix(self: *Self) ?LiteralIntSuffixResult {
        if (self.match('u') or self.match('U')) {
            const width = self.literalIntSuffixWidth() orelse 32;
            return LiteralIntSuffixResult{
                .width = width,
                .signed = false,
            };
        } else if (self.literalIntSuffixWidth()) |width| {
            const unsigned = self.match('u') or self.match('U');
            return LiteralIntSuffixResult{
                .width = width,
                .signed = !unsigned,
            };
        } else return null;
    }

    fn literalIntSuffixWidth(self: *Self) ?u8 {
        // NOTE: The order matters
        if (self.match("ll") or self.match("LL")) {
            return 64;
        } else if (self.match('z') or self.match('Z')) {
            return 64;
        } else if (self.match('l') or self.match('L')) {
            return 32;
        }
        return null;
    }

    const LiteralFloatBase = enum { dec, hex };

    fn literalFloat(self: *Self, comptime base: LiteralFloatBase) !?Token {
        self.pushIdx();
        defer self.popIdx();

        const digitFn = switch (base) {
            .dec => ch.decimal,
            .hex => ch.hex,
        };

        if (self.literalDigitSequence(digitFn)) {
            if (self.match('.')) {
                optional(self.literalDigitSequence(digitFn));
                switch (base) {
                    .dec => optional(self.literalFloatExponent(base)),
                    .hex => if (!self.expect(self.literalFloatExponent(base))) return null,
                }

                const width = self.literalFloatSuffix() orelse 64;
                const value = try util.parseFloat(self.tokenStr());
                return Token{ .literal_float = .{ .width = width, .value = value } };
            } else {
                if (!self.expect(self.literalFloatExponent(base))) return null;
                const width = self.literalFloatSuffix() orelse 64;
                const value = try util.parseFloat(self.tokenStr());
                return Token{ .literal_float = .{ .width = width, .value = value } };
            }
        }
        if (self.match('.')) {
            if (!self.expect(self.literalDigitSequence(digitFn))) return null;
            switch (base) {
                .dec => optional(self.literalFloatExponent(base)),
                .hex => if (!self.expect(self.literalFloatExponent(base))) return null,
            }

            const width = self.literalFloatSuffix() orelse 64;
            const value = try util.parseFloat(self.tokenStr());
            return Token{ .literal_float = .{ .width = width, .value = value } };
        }
        return null;
    }

    fn literalFloatExponent(self: *Self, comptime base: LiteralFloatBase) bool {
        const has_exponent, const digitFn = switch (base) {
            .dec => .{ self.match('e') or self.match('E'), ch.decimal },
            .hex => .{ self.match('p') or self.match('P'), ch.hex },
        };

        if (has_exponent) {
            optional(self.match('+') or self.match('-'));
            return self.literalDigitSequence(digitFn);
        }
        return false;
    }

    fn literalFloatSuffix(self: *Self) ?u8 {
        if (self.match('f') or self.match('F')) {
            return 32;
        } else if (self.match('l') or self.match('L')) {
            return 128;
        }
        return null;
    }

    fn literalDigitSequence(self: *Self, digitFn: MatchFn) bool {
        self.pushIdx();
        defer self.popIdx();

        if (self.match(digitFn)) while (true) {
            if (self.match(digitFn)) {
                continue;
            } else if (self.match("'")) {
                while (self.match("'")) {}
                if (!self.expect(digitFn)) return false;
            } else {
                return true;
            }
        };
        return false;
    }

    fn tokenStr(self: Self) []const u8 {
        return self.source[self.token_start..self.idx];
    }

    fn pushIdx(self: *Self) void {
        self.idx_stack.append(self.idx) catch @panic("lexer allocator out of memory");
    }

    fn popIdx(self: *Self) void {
        if (self.idx_stack.popOrNull() == null) @panic("popIdx(): Forgot to pushIdx()");
    }

    const MatchFn = fn ([]const u8) ?usize;

    /// Takes in a `u8`, `[]const u8`, or `MatchFn` and checks if the current head of the
    /// input string starts with the sequence of chars described by it. If yes, it
    /// advances the head pointer.
    fn match(self: *Self, what: anytype) bool {
        if (self.idx >= self.source.len) return false;
        const Type = @TypeOf(what);

        if (comptime util.isConvertibleTo(Type, u8)) {
            if (self.source[self.idx] == what) {
                self.idx += 1;
                return true;
            }
        } else if (comptime util.isConvertibleTo(Type, []const u8)) {
            if (std.mem.startsWith(u8, self.source[self.idx..], what)) {
                self.idx += @intCast(what.len);
                return true;
            }
        } else if (Type == MatchFn) {
            if (what(self.source[self.idx..])) |len| {
                self.idx += @intCast(len);
                return true;
            }
        } else {
            @compileError("unexpected type '" ++ @typeName(@TypeOf(what)) ++
                "', match() only accepts values of type: 'u8', '[]const u8'," ++
                " and 'MatchFn' (aka 'fn ([]const u8) ?usize')");
        }
        return false;
    }

    // TODO: This is now mostly wrong
    /// Takes in a `u8`, `[]const u8`, or `MatchFn` and checks if the current head of the
    /// input string starts with the sequence of chars described by it. It can also
    /// accept a plain `bool` representing a successful match in case your function can't
    /// conform to the `MatchFn` interface.
    /// If there is no match, it rolls back the head index to the previous 'match frame',
    /// same as `self.popIdx()`. A new match frame may be started by using
    /// `self.pushIdx()` and must be ended on all possible code paths. This means that
    /// if you have `if`s in your code, you may still need to call `self.popIdx()` explicitly.
    /// ```zig
    /// fn matchHello(self: *Self) bool {
    ///     self.pushIdx();
    ///     return self.expect(self.match('H') or self.match('h')) and self.expect("ello");
    /// }
    /// ```
    fn expect(self: *Self, what: anytype) if (@typeInfo(@TypeOf(what)) != .Optional) bool else @TypeOf(what) {
        const Type = @TypeOf(what);
        const stack = self.idx_stack.items;

        if (Type == bool) {
            if (what == false) self.idx = stack[stack.len - 1];
            return what;
        } else if (@typeInfo(Type) == .Optional) {
            if (what == null) self.idx = stack[stack.len - 1];
            return what;
        } else if (comptime Type == MatchFn or
            util.isConvertibleTo(Type, u8) or
            util.isConvertibleTo(Type, []const u8))
        {
            const ok = self.match(what);
            if (ok == false) self.idx = stack[stack.len - 1];
            return ok;
        } else @compileError("unexpected type '" ++ @typeName(Type) ++
            "', expect() only accepts values of type: 'bool', 'u8', " ++
            "'[]const u8', and 'MatchFn' (aka 'fn ([]const u8) ?usize')");
    }

    /// Takes in a `u8`, `[]const u8`, or `MatchFn` and checks if the current head of the
    /// input string starts with the sequence of chars described by it.
    ///
    /// See also: [Lexer.pushIdx()](src/lexer.zig), [Lexer.popIdx()](src/lexer.zig)
    fn peek(self: *Self, what: anytype) bool {
        if (self.idx >= self.source.len) return false;
        const T = @TypeOf(what);

        if (comptime util.isConvertibleTo(T, u8)) {
            return self.source[self.idx] == what;
        } else if (comptime util.isConvertibleTo(T, []const u8)) {
            return std.mem.startsWith(u8, self.source[self.idx..], what);
        } else if (comptime util.isConvertibleTo(T, MatchFn)) {
            return what(self.source[self.idx..]) != null;
        } else {
            @compileError("unexpected type '" ++ @typeName(@TypeOf(what)) ++
                "', peek() only accepts values of type: 'u8', '[]const u8'," ++
                " and 'MatchFn' (aka 'fn ([]const u8) ?usize')");
        }
    }

    /// This has no practical function, it is here just to be sprinkled through the code
    /// to make it more readable by signaling intent rather than ignoring a random value.
    /// ```zig
    ///     optional(self.literalDigitSequence(ch.hex));
    /// ```
    ///
    /// See also: [Lexer.expect()](src/lexer.zig)
    fn optional(ok: anytype) void {
        _ = ok;
    }
};

test Lexer {
    const testing = std.testing;
    const source =
        \\1e10
        \\1e-5L
        \\1.
        \\1.e-2
        \\3.14
        \\.1f
        \\0.1e-1L
        \\0x1ffp10
        \\0X0p-1
        \\0x1.p0
        \\0xf.p-1
        \\0x0.123p-1
        \\0xa.bp10l
    ;

    const expected = [_]f128{
        1e10,
        1e-5,
        1.0,
        1.0e-2,
        3.14,
        0.1,
        0.1e-1,
        0x1ffp10,
        0x0p-1,
        0x1.p0,
        0xf.p-1,
        0x0.123p-1,
        0xa.bp10,
    };

    var lexer = Lexer.init(testing.allocator, source);
    defer lexer.deinit();

    var i = @as(usize, 0);
    while (try lexer.next()) |token| {
        const actual = token.literal_float.value;
        try testing.expectApproxEqRel(expected[i], actual, 1e-16); // TODO: Improve the precision
        i += 1;
    }
}
