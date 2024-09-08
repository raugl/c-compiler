const std = @import("std");
const util = @import("util.zig");
const kw = @import("keyword.zig");
const op = @import("operator.zig");
const tk = @import("token.zig");

const Token = tk.Token;
const LocToken = tk.LocToken;

/// A wrapper around `Lexer` that opens and manages a file for lexing
pub const FileLexer = struct {
    const Self = @This();
    lexer: Lexer,
    file: std.fs.File,
    ptr: []align(std.mem.page_size) u8,

    pub fn init(alloc: std.mem.Allocator, absolute_path: []const u8) !Self {
        return withWritter(alloc, absolute_path, std.io.getStdErr().writer());
    }

    pub fn withWritter(
        alloc: std.mem.Allocator,
        absolute_path: []const u8,
        writter: std.fs.File.Writer,
    ) !Self {
        // NOTE: I'm only using mmap so I don't need to load the whole file into memory
        // manually. The kernel can inteligently manage loading/unloading of pages,
        // and I still get a contiguous slice
        const file = try std.fs.openFileAbsolute(absolute_path, .{});
        const md = try file.metadata();
        const ptr = try std.posix.mmap(null, md.size(), std.posix.PROT.READ, .{ .TYPE = .SHARED }, file.handle, 0);
        const filename = std.fs.path.basename(absolute_path);

        return Self{
            .file = file,
            .ptr = ptr,
            .lexer = Lexer.withWriter(alloc, ptr, filename, writter),
        };
    }

    pub fn deinit(self: *Self) void {
        self.lexer.deinit();
        std.posix.munmap(self.ptr);
        self.file.close();
    }

    pub fn next(self: *Self) !?LocToken {
        return self.lexer.next();
    }

    pub fn hadErrors(self: Self) bool {
        return self.lexer.hadErrors();
    }
};

pub const Lexer = struct {
    const Self = @This();

    source: []const u8,
    filename: []const u8,
    idx: u32 = 0,
    line_start: u32 = 0,
    token_start: u32 = 0,
    line_nr: u16 = 0,
    had_errors: bool = false,
    first_on_line: bool = true, // TODO: for macros

    writer: std.fs.File.Writer,
    arena: std.heap.ArenaAllocator,
    logs: std.ArrayListUnmanaged(LogRecord),

    const LogRecord = struct {
        msg: []const u8,
        severity: []const u8,
        color: []const u8,
        idx: ?u32,
    };

    /// Deinitialize with `deinit`.
    pub fn init(alloc: std.mem.Allocator, source: []const u8, filename: []const u8) Self {
        return withWriter(alloc, source, filename, std.io.getStdErr().writer());
    }

    /// The writer used to print output to. `init` defaults to stderr
    pub fn withWriter(
        alloc: std.mem.Allocator,
        source: []const u8,
        filename: []const u8,
        writer: std.fs.File.Writer,
    ) Self {
        return Self{
            .source = source,
            .filename = filename,
            .writer = writer,
            .arena = std.heap.ArenaAllocator.init(alloc),
            .logs = std.ArrayListUnmanaged(LogRecord){},
        };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    pub fn hadErrors(self: Self) bool {
        return self.had_errors;
    }

    pub fn next(self: *Self) !?LocToken {
        self.consumeWhitespace();

        // NOTE: The order matters
        const token =
            self.literalBool() orelse
            self.keyword() orelse
            self.identifier() orelse
            self.preproc() orelse
            try self.comment() orelse
            try self.literalStr() orelse
            try self.literalChar() orelse
            try self.literalNumeric() orelse
            self.operator();

        if (token == null) {
            const source = self.source[self.idx..];
            if (std.mem.indexOfNone(u8, source, &std.ascii.whitespace) != null) {
                self.idx += @intCast(std.mem.indexOfAny(u8, source, "\r\n") orelse self.source.len);
                try self.err("invalid bytes", null, .{});
            }
        }

        for (self.logs.items) |rec| {
            try self.log(rec);
        }
        _ = self.arena.reset(.retain_capacity);
        self.logs.clearAndFree(self.arena.allocator());

        return LocToken{
            .line_nr = self.line_nr + 1,
            .col_nr = @intCast(self.line_start - self.token_start),
            .token = token orelse return null,
        };
    }

    // FIXME: This doesn't support stand-alone '\r' as newlines
    fn consumeWhitespace(self: *Self) void {
        const new_idx = std.mem.indexOfNonePos(u8, self.source, self.idx, &std.ascii.whitespace);
        self.idx = @intCast(new_idx orelse self.source.len);

        for (self.source[self.token_start..self.idx], 0..) |ch, i| {
            if (ch == '\n') {
                self.line_start = self.token_start + @as(u32, @intCast(i + 1));
                self.line_nr += 1;
            }
        }
        self.token_start = self.idx;
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
        if (self.match('_') or self.match(util.alphabetic)) {
            while (self.match('_') or self.match(util.alphaNumeric)) {}
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
            if (std.mem.indexOfPos(u8, self.source, self.idx, "*/")) |end_idx| {
                self.idx = @intCast(end_idx + 2);
                return Token{ .comment = self.tokenStr() };
            }
            self.idx = @intCast(self.source.len);
            try self.err("unterminated block comment", null, .{});
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
        if (self.first_on_line and self.match('#')) {
            optional(self.match(util.whitespace));

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
            } else if (self.match("endif")) {
                return Token{ .preproc = .endif };
            }
        }
        return null;
    }

    // TODO:
    fn literalStr(self: *Self) !?Token {
        if (self.match('"')) {
            if (std.mem.indexOfScalarPos(u8, self.source, self.idx, '"')) |end_idx| {
                self.idx = @intCast(end_idx + 1);
            } else {
                const end_idx = std.mem.indexOfAnyPos(u8, self.source, self.idx, "\r\n");
                self.idx = @intCast(end_idx orelse self.source.len);
                try self.err("missing terminating \" character", null, .{});
            }
            return Token{ .literal_str = self.tokenStr() };
        }
        return null;
    }

    // TODO:
    fn literalChar(self: *Self) !?Token {
        if (self.match("'")) {
            if (std.mem.indexOfScalarPos(u8, self.source, self.idx, '\'')) |end_idx| {
                self.idx = @intCast(end_idx + 1);
            } else {
                const end_idx = std.mem.indexOfAnyPos(u8, self.source, self.idx, "\r\n");
                self.idx = @intCast(end_idx orelse self.source.len);
                try self.err("missing terminating ' character", null, .{});
            }
            return Token{ .literal_str = self.tokenStr() };
        }
        return null;
    }

    fn literalNumeric(self: *Self) !?Token {
        // NOTE: The order matters
        if (self.match("0b") or self.match("0B")) {
            return try self.literalInt(util.binary);
        }
        if (self.match("0x") or self.match("0X")) {
            return try self.literalNumericHex();
        }
        if (try self.literalFloat()) |token| {
            return token;
        }
        if (self.peek(util.nonZero)) {
            return self.literalInt(util.decimal);
        }
        if (self.peek('0')) {
            return self.literalInt(util.octal);
        }
        return null;
    }

    fn literalInt(self: *Self, digitFn: MatchFn) !?Token {
        if (!try self.literalDigitSequence(digitFn)) {
            try self.err("no digits in literal integer constant", self.idx - 1, .{});
        }
        const value = try util.parseInt(self.tokenStr());
        const suffix = try self.literalIntSuffix() orelse LiteralIntSuffixResult{};

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

    fn literalIntSuffix(self: *Self) !?LiteralIntSuffixResult {
        const start_idx = self.idx;
        const res = blk: {
            if (self.match('u') or self.match('U')) {
                const width = self.literalIntSuffixWidth() orelse 32;
                break :blk LiteralIntSuffixResult{
                    .width = width,
                    .signed = false,
                };
            } else if (self.literalIntSuffixWidth()) |width| {
                const unsigned = self.match('u') or self.match('U');
                break :blk LiteralIntSuffixResult{
                    .width = width,
                    .signed = !unsigned,
                };
            }
            break :blk null;
        };
        var has_extra = false;
        while (self.match('_') or self.match(util.alphaNumeric)) has_extra = true;

        if (has_extra) {
            const suffix = self.source[start_idx..self.idx];
            try self.err("invalid suffix \"{s}\" for integer constant", start_idx, .{suffix});
        }
        return res;
    }

    fn literalIntSuffixWidth(self: *Self) ?u8 {
        // NOTE: The order matters
        if (self.match("ll") or self.match("LL")) {
            return 64;
        } else if (self.match('l') or self.match('L')) {
            return 32;
        } else if (self.match('z') or self.match('Z')) {
            return 64;
        }
        return null;
    }

    const LiteralFloatBase = enum { dec, hex };

    fn literalFloat(self: *Self) !?Token {
        const rollback_idx = self.idx;

        if (try self.literalDigitSequence(util.decimal)) {
            if (self.match('.')) {
                optional(try self.literalDigitSequence(util.decimal));
                optional(try self.literalFloatExponent(.dec));
                const width = try self.literalFloatSuffix() orelse 64;
                const value = try util.parseFloat(self.tokenStr());
                return Token{ .literal_float = .{
                    .width = width,
                    .value = value,
                } };
            } else {
                if (!try self.literalFloatExponent(.dec)) {
                    self.idx = rollback_idx;
                    return null;
                }
                const width = try self.literalFloatSuffix() orelse 64;
                const value = try util.parseFloat(self.tokenStr());
                return Token{ .literal_float = .{
                    .width = width,
                    .value = value,
                } };
            }
        }
        if (self.match('.')) {
            if (!try self.literalDigitSequence(util.decimal)) {
                self.idx = rollback_idx;
                return null;
            }
            optional(try self.literalFloatExponent(.dec));
            const width = try self.literalFloatSuffix() orelse 64;
            const value = try util.parseFloat(self.tokenStr());
            return Token{ .literal_float = .{
                .width = width,
                .value = value,
            } };
        }
        return null;
    }

    fn literalNumericHex(self: *Self) !?Token {
        const rollback_idx = self.idx;

        if (try self.literalDigitSequence(util.hex)) {
            if (self.match('.')) {
                optional(try self.literalDigitSequence(util.hex));
                if (!try self.literalFloatExponent(.hex)) {
                    try self.err("hexadecimal floating constants require an exponent", null, .{});
                }
                const value = try util.parseFloat(self.tokenStr());
                const width = try self.literalFloatSuffix() orelse 64;
                return Token{ .literal_float = .{
                    .width = width,
                    .value = value,
                } };
            } else {
                if (!try self.literalFloatExponent(.hex)) {
                    self.idx = rollback_idx;
                    return self.literalInt(util.hex);
                }
                const value = try util.parseFloat(self.tokenStr());
                const width = try self.literalFloatSuffix() orelse 64;
                return Token{ .literal_float = .{
                    .width = width,
                    .value = value,
                } };
            }
        }
        if (self.match('.')) {
            if (!try self.literalDigitSequence(util.hex)) {
                try self.err("no digits in hexadecimal floating constant", self.idx - 1, .{});
            }
            if (!try self.literalFloatExponent(.hex)) {
                try self.err("hexadecimal floating constants require an exponent", null, .{});
            }
            const value = try util.parseFloat(self.tokenStr());
            const width = try self.literalFloatSuffix() orelse 64;
            return Token{ .literal_float = .{
                .width = width,
                .value = value,
            } };
        }
        return self.literalInt(util.hex);
    }

    fn literalFloatExponent(self: *Self, comptime base: LiteralFloatBase) !bool {
        const has_exponent = switch (base) {
            .dec => self.match('e') or self.match('E'),
            .hex => self.match('p') or self.match('P'),
        };

        if (has_exponent) {
            optional(self.match('+') or self.match('-'));
            if (!try self.literalDigitSequence(util.decimal)) {
                try self.err("exponent has no digits", self.idx - 1, .{});
            }
        }
        return has_exponent;
    }

    fn literalFloatSuffix(self: *Self) !?u8 {
        const start_idx = self.idx;
        const res: ?u8 = blk: {
            if (self.match('f') or self.match('F')) {
                break :blk 32;
            } else if (self.match('l') or self.match('L')) {
                break :blk 128;
            }
            break :blk null;
        };
        var has_extra = false;
        while (self.match('_') or self.match(util.alphaNumeric)) has_extra = true;

        if (has_extra) {
            const suffix = self.source[start_idx..self.idx];
            try self.err("invalid suffix \"{s}\" for floating constant", start_idx, .{suffix});
        }
        return res;
    }

    fn literalDigitSequence(self: *Self, digitFn: MatchFn) !bool {
        if (self.match("'")) {
            try self.err("digit separator outside digit sequence", self.idx - 1, .{});
        }
        var has_digits = false;
        while (true) {
            if (self.match(digitFn)) {
                has_digits = true;
            } else if (self.match("'")) {
                while (self.match("'")) {}
                if (!self.peek(digitFn)) {
                    try self.err("digit separator outside digit sequence", self.idx - 1, .{});
                    break;
                }
            } else break;
        }
        return has_digits;
    }

    fn tokenStr(self: Self) []const u8 {
        return self.source[self.token_start..self.idx];
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
    ///     optional(self.literalDigitSequence(util.hex));
    /// ```
    ///
    /// See also: [Lexer.expect()](src/lexer.zig)
    fn optional(ok: anytype) void {
        _ = ok;
    }

    const esc = struct {
        const err = bold ++ "\x1b[91m";
        const warn = bold ++ "\x1b[95m";
        const note = bold ++ "\x1b[96m";
        const bold = "\x1b[1m";
        const reset = "\x1b[m";
    };

    fn note(self: Self, comptime fmt: []const u8, idx: ?u32, args: anytype) !void {
        const msg = try std.fmt.allocPrint(self.arena.allocator(), fmt, args);
        try self.logs.append(self.arena.allocator(), .{
            .msg = msg,
            .idx = idx,
            .severity = "note",
            .color = esc.note,
        });
    }

    fn warn(self: Self, comptime fmt: []const u8, idx: ?u32, args: anytype) !void {
        const msg = try std.fmt.allocPrint(self.arena.allocator(), fmt, args);
        try self.logs.append(self.arena.allocator(), .{
            .msg = msg,
            .idx = idx,
            .severity = "warning",
            .color = esc.warn,
        });
    }

    fn err(self: *Self, comptime fmt: []const u8, idx: ?u32, args: anytype) !void {
        self.had_errors = true;
        const msg = try std.fmt.allocPrint(self.arena.allocator(), fmt, args);
        try self.logs.append(self.arena.allocator(), .{
            .msg = msg,
            .idx = idx,
            .severity = "error",
            .color = esc.err,
        });
    }

    // FIXME: tabs break this
    fn log(self: Self, rec: LogRecord) !void {
        const column = self.token_start - self.line_start;
        try self.writer.print(esc.bold ++ "{[file]s}:{[row_nr]}:{[col_nr]}: {[color]s}{[severity]s}:" ++ esc.reset ++ " {[msg]s}\n", .{
            .file = self.filename,
            .row_nr = self.line_nr + 1,
            .col_nr = column + 1,
            .msg = rec.msg,
            .color = rec.color,
            .severity = rec.severity,
        });

        const line_end = std.mem.indexOfAnyPos(u8, self.source, self.token_start, "\r\n") orelse self.source.len;
        const line = self.source[self.line_start..line_end];
        const hgl_start = column; // highlight start
        const hgl_end = @min(line_end, self.idx) - self.line_start;
        try self.writer.print("{[row_nr]:>5} | {[pre]s}{[color]s}{[highlight]s}" ++ esc.reset ++ "{[post]s}\n", .{
            .row_nr = self.line_nr + 1,
            .pre = line[0..hgl_start],
            .highlight = line[hgl_start..hgl_end],
            .post = line[hgl_end..],
            .color = rec.color,
        });

        const tabs = std.mem.count(u8, line, "\t") * 7; // assume 8-wide tabs in the terminal
        const caret = (rec.idx orelse self.token_start) - self.line_start;
        const padding = " " ** 256;
        const squirly = "~" ** 256;
        try self.writer.print("      | {[padding]s}{[color]s}{[pre]s}^{[post]s}" ++ esc.reset ++ "\n", .{
            .padding = padding[0..(hgl_start + tabs)],
            .pre = squirly[0..(caret - hgl_start)],
            .post = squirly[0..(hgl_end - caret -| 1)],
            .color = rec.color,
        });
    }
};

test "literalFloat" {
    const expectFloat = struct {
        fn Fn(lexer_: *Lexer, expected: comptime_float) !void {
            const tolerance = 1e-16;
            const actual = (try lexer_.next()).?.literal_float.value;
            try std.testing.expectApproxEqAbs(expected, actual, tolerance);
        }
    }.Fn;
    const source =
        \\1e10 1e-5L 1. 1.e-2 3.14 .1f 0.1e-1L 0x1ffp10 0X0p-1 0x1.p0 0xf.p-1 0x0.123p-1 0xa.bp10l
    ;
    var lexer = Lexer.init(std.testing.allocator, source);
    defer lexer.deinit();

    try expectFloat(&lexer, 1e10);
    try expectFloat(&lexer, 1e-5);
    try expectFloat(&lexer, 1.0);
    try expectFloat(&lexer, 1.0e-2);
    try expectFloat(&lexer, 3.14);
    try expectFloat(&lexer, 0.1);
    try expectFloat(&lexer, 0.1e-1);
    try expectFloat(&lexer, 0x1ffp10);
    try expectFloat(&lexer, 0x0p-1);
    try expectFloat(&lexer, 0x1.p0);
    try expectFloat(&lexer, 0xf.p-1);
    try expectFloat(&lexer, 0x0.123p-1);
    try expectFloat(&lexer, 0xa.bp10);
}

test "consumeWhitespace" {
    const source =
        \\  foo bar
        \\ foo       bar
        \\ /*
        \\  *
        \\  */
        \\
    ;
    var self = Lexer.init(std.testing.allocator, source);
    defer self.deinit();

    try std.testing.expectEqualDeep(Token{ .identifier = "foo" }, self.next());
    try std.testing.expectEqualDeep(.{ 0, 0, 2, 5 }, .{ self.line_nr, self.line_start, self.token_start, self.idx });

    try std.testing.expectEqualDeep(Token{ .identifier = "bar" }, self.next());
    try std.testing.expectEqualDeep(.{ 0, 0, 6, 9 }, .{ self.line_nr, self.line_start, self.token_start, self.idx });

    try std.testing.expectEqualDeep(Token{ .identifier = "foo" }, self.next());
    try std.testing.expectEqualDeep(.{ 1, 10, 11, 14 }, .{ self.line_nr, self.line_start, self.token_start, self.idx });

    try std.testing.expectEqualDeep(Token{ .identifier = "bar" }, self.next());
    try std.testing.expectEqualDeep(.{ 1, 10, 21, 24 }, .{ self.line_nr, self.line_start, self.token_start, self.idx });

    try std.testing.expectEqualDeep(Token{ .comment = "/*\n  *\n  */" }, self.next());
    try std.testing.expectEqualDeep(.{ 2, 25, 26, 37 }, .{ self.line_nr, self.line_start, self.token_start, self.idx });
}
