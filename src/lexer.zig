const std = @import("std");
const util = @import("util.zig");
const kw = @import("keyword.zig");
const op = @import("operator.zig");
const tk = @import("token.zig");
const wc = @import("wcwidth");

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
        // TODO: Crunching the numbers, a one million line file would only take
        // about 40MB which honestly is nothing. I could easily load it whole into
        // memory and not care about it. I could also do some char replacements
        // that would make lexing a lot simpler, like tabs and newlines.
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
    line_nr: u16 = 1,
    had_errors: bool = false,
    first_on_line: bool = true, // TODO: for macros

    writer: std.fs.File.Writer,
    alloc: std.mem.Allocator,
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
            .alloc = alloc,
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
        self.token_start = self.idx;
        defer self.first_on_line = false;

        // NOTE: The order matters
        const token =
            self.preproc() orelse
            try self.comment() orelse
            try self.literalStr() orelse
            try self.literalChar() orelse
            try self.literalNumeric() orelse
            self.alphabetic() orelse
            self.operator();

        if (token == null) {
            const source = self.source[self.idx..];
            if (std.mem.indexOfNone(u8, source, &std.ascii.whitespace) != null) {
                self.idx += @intCast(std.mem.indexOfAny(u8, source, "\r\n") orelse self.source.len);
                try self.err("invalid bytes", null, .{});
            }
        }

        var iter = std.mem.reverseIterator(self.logs.items);
        while (iter.next()) |rec| try self.log(rec);
        // for (self.logs.items) |rec| try self.log(rec);

        self.logs.clearAndFree(self.arena.allocator());
        _ = self.arena.reset(.retain_capacity);

        const col_nr = try wc.sliceWidth(self.source[self.line_start..self.token_start]);
        return LocToken{
            .line_nr = self.line_nr,
            .col_nr = @as(u16, @intCast(col_nr)) + 1,
            .token = token orelse return null,
        };
    }

    // FIXME: This doesn't support stand-alone '\r' as newlines
    fn consumeWhitespace(self: *Self) void {
        const control_code = std.ascii.control_code;

        while (self.idx < self.source.len) {
            switch (self.source[self.idx]) {
                '\\' => if (self.match("\\\n")) {
                    self.first_on_line = true;
                    self.line_start = self.idx;
                    self.line_nr += 1;
                },
                '\n' => {
                    self.idx += 1;
                    self.first_on_line = true;
                    self.line_start = self.idx;
                    self.line_nr += 1;
                },
                ' ', '\t', '\r', control_code.vt, control_code.ff => self.idx += 1,
                else => break,
            }
        }
    }

    fn alphabetic(self: *Self) ?Token {
        if (self.match('_') or self.match(util.alphabetic)) {
            while (self.match('_') or self.match(util.alphaNumeric)) {}
            const source = self.source[self.token_start..self.idx];

            if (std.mem.eql(u8, source, "true")) {
                return Token{ .literal_bool = true };
            }
            if (std.mem.eql(u8, source, "false")) {
                return Token{ .literal_bool = false };
            }
            if (kw.parseKeyword(source)) |res| {
                return Token{ .keyword = res.kw };
            }
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

    /// This returns an owned slice with the proccessed string bytes. The caller owns the memory.
    fn literalStr(self: *Self) !?Token {
        if (self.match('"')) {
            return Token{ .literal_str = .{ .char = try self.literalStrImpl(u8) } };
        }
        if (self.match("u8\"")) {
            return Token{ .literal_str = .{ .utf8 = try self.literalStrImpl(u8) } };
        }
        if (self.match("u\"")) {
            return Token{ .literal_str = .{ .utf16 = try self.literalStrImpl(u16) } };
        }
        if (self.match("U\"")) {
            return Token{ .literal_str = .{ .utf32 = try self.literalStrImpl(u32) } };
        }
        if (self.match("L\"")) {
            // FIXME: For whatever-the-fuck reason `wchar_t` on Windows is only 16 bits wide
            return Token{ .literal_str = .{ .wchar = try self.literalStrImpl(u32) } };
        }
        return null;
    }

    fn literalStrImpl(self: *Self, comptime T: type) ![]const T {
        const max_value = (1 << @bitSizeOf(T)) - 1;
        var result = std.ArrayList(T).init(self.alloc);
        defer result.deinit();

        while (self.idx < self.source.len) {
            const seq_start = self.idx;
            if (self.match('"')) {
                break;
            }
            if (self.match("\\\n")) {
                continue;
            }
            if (self.peek('\n')) {
                try self.err("missing terminating \" character", null, .{});
                break;
            }
            if (try self.escapeSequence()) |seq| {
                switch (seq.kind) {
                    .hex, .octal => {
                        if (seq.num > max_value) {
                            try self.warn("{s} escape sequence out of range", seq_start, .{@tagName(seq.kind)});
                            try result.append(@truncate(0xffff_ffff));
                        } else {
                            try result.append(@truncate(seq.num));
                        }
                    },
                    .utf16, .utf32 => try self.unicodeEncode(T, seq.num, &result, seq_start),
                    .simple => try result.append(@truncate(seq.num)),
                }
            } else {
                const len = try std.unicode.utf8ByteSequenceLength(self.source[self.idx]);
                self.idx += len;
                const source = self.source[seq_start..self.idx];
                const ch = try std.unicode.utf8Decode(source);
                try self.unicodeEncode(T, @intCast(ch), &result, seq_start);
            }
        }
        // NOTE: Don't include the null terminator as that will be added by the
        // preprocessor after string concatenation.
        return result.toOwnedSlice();
    }

    const EscapeKind = enum { simple, octal, hex, utf16, utf32 };
    const EscapeSequenceResult = struct {
        kind: EscapeKind,
        num: u32,
    };

    fn escapeSequence(self: *Self) !?EscapeSequenceResult {
        const seq_start = self.idx;

        if (self.match('\\')) {
            simple: {
                const num: u8 = switch (self.source[self.idx]) {
                    '\'' => 0x27,
                    '"' => 0x22,
                    '?' => 0x3f,
                    '\\' => 0x5c,
                    'a' => 0x07,
                    'b' => 0x08,
                    'f' => 0x0c,
                    'n' => 0x0a,
                    'r' => 0x0d,
                    't' => 0x09,
                    'v' => 0x0b,
                    else => break :simple,
                };
                self.idx += 1;
                return .{ .kind = .simple, .num = num };
            }
            numeric: {
                const kind: EscapeKind = blk: {
                    if (self.peek(util.octal)) break :blk .octal;
                    if (self.match('x')) break :blk .hex;
                    if (self.match('u')) break :blk .utf16;
                    if (self.match('U')) break :blk .utf32;
                    break :numeric;
                };
                const digits_start = self.idx;

                var len = @as(u8, 0);
                if (kind == .octal) {
                    while (self.match(util.octal)) len += 1;
                } else {
                    while (self.match(util.hex)) len += 1;
                }
                switch (kind) {
                    .octal => if (len > 3) {
                        try self.err("octal escape sequence must have at most 3 digits", seq_start, .{});
                    },
                    .utf16 => if (len != 4) {
                        try self.err("universal character name must have exactly 4 digits", seq_start, .{});
                    },
                    .utf32 => if (len != 8) {
                        try self.err("universal character name must have exactly 8 digits", seq_start, .{});
                    },
                    .simple, .hex => {},
                }

                const base: u8 = if (kind == .octal) 8 else 16;
                const source = self.source[digits_start..self.idx];
                var num = @as(u32, 0);

                for (source) |ch| {
                    const digit = std.fmt.charToDigit(ch, base) catch break;
                    num *%= base;
                    num +%= digit;
                }
                return .{ .kind = kind, .num = num };
            }
            const unknown_seq = self.source[seq_start..(self.idx + 1)];
            try self.err("unknown escape sequence '{s}'", seq_start, .{unknown_seq});
            return .{ .kind = .simple, .num = 0xff };
        }
        return null;
    }

    fn unicodeEncode(
        self: *Self,
        comptime T: type,
        ch: u32,
        out: *std.ArrayList(T),
        seq_start: u32,
    ) !void {
        switch (T) {
            u8 => utf8Encode(ch, out) catch |err_| {
                if (err_ == error.CodepointTooLarge) {
                    try self.warn("codepoint outside the UCS codespace", seq_start, .{});
                }
            },
            u16 => utf16Encode(ch, out) catch |err_| {
                if (err_ == error.CodepointTooLarge) {
                    try self.warn("codepoint outside the UCS codespace", seq_start, .{});
                }
            },
            u32 => try out.append(ch),
            else => @compileError("unexpected char type " ++ @typeName(T)),
        }
    }

    fn utf8Encode(cp: u32, out: *std.ArrayList(u8)) !void {
        const utf8SeqLen = std.unicode.utf8CodepointSequenceLength;
        const length = try utf8SeqLen(@truncate(@min(cp, 0x10_ffff)));
        switch (length) {
            1 => try out.append(@intCast(cp)), // Can just do 0 + codepoint for initial range
            2 => {
                try out.append(@intCast(0b11000000 | (cp >> 6)));
                try out.append(@intCast(0b10000000 | (cp & 0b111111)));
            },
            3 => {
                try out.append(@intCast(0b11100000 | (cp >> 12)));
                try out.append(@intCast(0b10000000 | ((cp >> 6) & 0b111111)));
                try out.append(@intCast(0b10000000 | (cp & 0b111111)));
            },
            4 => {
                try out.append(@intCast(0b11110000 | (cp >> 18)));
                try out.append(@intCast(0b10000000 | ((cp >> 12) & 0b111111)));
                try out.append(@intCast(0b10000000 | ((cp >> 6) & 0b111111)));
                try out.append(@intCast(0b10000000 | (cp & 0b111111)));
            },
            else => unreachable,
        }
    }

    fn utf16Encode(cp: u32, out: *std.ArrayList(u16)) !void {
        const utf16SeqLen = std.unicode.utf16CodepointSequenceLength;
        const length = try utf16SeqLen(@truncate(@min(cp, 0x10_ffff)));
        switch (length) {
            1 => try out.append(@intCast(cp)), // Can just do 0 + codepoint for initial range
            2 => {
                const lead_offset = 0xD800 - (0x10000 >> 10);
                try out.append(@intCast(lead_offset + (cp >> 10)));
                try out.append(@intCast(0xDC00 + (cp & 0x3FF)));
            },
            else => unreachable,
        }
    }

    fn literalChar(self: *Self) !?Token {
        if (self.match("'")) return try self.literalCharImpl(u8, .char);
        if (self.match("u8'")) return try self.literalCharImpl(u8, .utf8);
        if (self.match("u'")) return try self.literalCharImpl(u16, .utf16);
        if (self.match("U'")) return try self.literalCharImpl(u32, .utf32);
        // FIXME: For whatever-the-fuck reason `wchar_t` on Windows is only 16 bits wide
        if (self.match("L'")) return try self.literalCharImpl(u32, .wchar);
        return null;
    }

    fn literalCharImpl(self: *Self, comptime T: type, comptime kind: CharKind) !Token {
        var result = @as(u32, 0);
        var num_chars = @as(u8, 0);
        const max_value = (1 << @bitSizeOf(T)) - 1;

        while (self.idx < self.source.len) {
            const seq_start = self.idx;
            if (self.match("'")) {
                break;
            }
            if (self.match("\\\n")) {
                continue;
            }
            if (self.peek('\n')) {
                try self.err("missing terminating ' character", null, .{});
                break;
            }
            if (try self.escapeSequence()) |seq| {
                switch (seq.kind) {
                    .hex, .octal => if (seq.num > max_value) {
                        try self.warn("{s} escape sequence out of range", seq_start, .{@tagName(seq.kind)});
                    },
                    .utf16, .utf32 => if (seq.num > max_value) {
                        try self.warn("character literal too large for its type", seq_start, .{});
                    },
                    .simple => {},
                }
                switch (kind) {
                    .char => {
                        if (num_chars == 1) {
                            try self.warn("using multi-character character literal", null, .{});
                        }
                        if (num_chars == 4) {
                            try self.warn("character literal too long for its type", null, .{});
                        }
                    },
                    .utf8, .utf16, .utf32 => if (num_chars == 1) {
                        try self.err("unicode character literal may not contain multiple characters", seq_start, .{});
                    },
                    .wchar => if (num_chars == 1) {
                        try self.err("wide character literal may not contain multiple characters", seq_start, .{});
                    },
                }
                accumChar(&result, @min(seq.num, max_value), kind);
                num_chars += 1;
            } else {
                const len = try std.unicode.utf8ByteSequenceLength(self.source[self.idx]);
                self.idx += len;
                const source = self.source[seq_start..self.idx];
                const cp = try std.unicode.utf8Decode(source);
                if (cp > max_value) {
                    try self.warn("character literal too large for its type", seq_start, .{});
                }
                accumChar(&result, @min(cp, max_value), kind);
                num_chars += 1;
            }
        }
        return switch (kind) {
            .char => Token{ .literal_int = .{
                .value = result,
                .width = 32,
                .signed = true,
            } },
            .utf8, .utf16, .utf32, .wchar => Token{ .literal_int = .{
                .value = result,
                .width = @bitSizeOf(T),
                .signed = false,
            } },
        };
    }

    const CharKind = enum { char, wchar, utf8, utf16, utf32 };

    fn accumChar(num: *u32, ch: u32, comptime kind: CharKind) void {
        // The behaviour for multi-character literals is unspecified, so I chose
        // to follow clang/gcc's behaviour as described here:
        // https://en.cppreference.com/w/c/language/character_constant#Notes
        switch (kind) {
            .char => {
                num.* <<= @bitSizeOf(u8);
                num.* += @as(u8, @intCast(ch));
            },
            .wchar, .utf8, .utf16, .utf32 => num.* = ch,
        }
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

    fn note(self: *Self, comptime fmt: []const u8, idx: ?u32, args: anytype) !void {
        const msg = try std.fmt.allocPrint(self.arena.allocator(), fmt, args);
        try self.logs.append(self.arena.allocator(), .{
            .msg = msg,
            .idx = idx,
            .severity = "note",
            .color = esc.note,
        });
    }

    fn warn(self: *Self, comptime fmt: []const u8, idx: ?u32, args: anytype) !void {
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
        const line_end = std.mem.indexOfScalarPos(u8, self.source, self.token_start, '\n') orelse self.source.len;
        const color_end = @min(self.idx, line_end);

        const pre_color = self.source[self.line_start..self.token_start];
        const colored = self.source[self.token_start..color_end];
        const aft_color = self.source[color_end..line_end];
        const pad_len = try wc.sliceWidth(pre_color);
        const col_nr = pad_len + 1;

        try self.writer.print(
            esc.bold ++ "{s}:{}:{}: {s}{s}:" ++ esc.reset ++ " {s}\n",
            .{ self.filename, self.line_nr, col_nr, rec.color, rec.severity, rec.msg },
        );

        try self.writer.print(
            "{:>5} | {s}{s}{s}" ++ esc.reset ++ "{s}\n",
            .{ self.line_nr, pre_color, rec.color, colored, aft_color },
        );

        const padding = " " ** 256;
        const squirly = "~" ** 256;
        const caret_idx = rec.idx orelse self.token_start;
        const pre_caret_len = try wc.sliceWidth(self.source[self.token_start..caret_idx]);
        const aft_caret_len = try wc.sliceWidth(self.source[caret_idx..color_end]) - 1;

        try self.writer.print("      | {s}{s}{s}^{s}" ++ esc.reset ++ "\n", .{
            padding[0..@intCast(pad_len)],
            rec.color,
            squirly[0..@intCast(pre_caret_len)],
            squirly[0..@intCast(aft_caret_len)],
        });
    }
};

test "literalFloat" {
    const expectFloat = struct {
        fn Fn(lexer_: *Lexer, expected: comptime_float) !void {
            const tolerance = 1e-16;
            const actual = (try lexer_.next()).?.token.literal_float.value;
            try std.testing.expectApproxEqAbs(expected, actual, tolerance);
        }
    }.Fn;
    const source =
        \\1e10 1e-5L 1. 1.e-2 3.14 .1f 0.1e-1L 0x1ffp10 0X0p-1 0x1.p0 0xf.p-1 0x0.123p-1 0xa.bp10l
    ;
    var lexer = Lexer.init(std.testing.allocator, source, "test.c");
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
    const Fn = struct {
        fn expectEqualIdxs(line_nr: u32, line_start: u32, token_start: u32, idx: u32, actual: Lexer) !void {
            try std.testing.expectEqualDeep(.{
                line_nr,
                line_start,
                token_start,
                idx,
            }, .{
                actual.line_nr,
                actual.line_start,
                actual.token_start,
                actual.idx,
            });
        }

        fn expectEqualToken(line_nr: u16, col_nr: u16, expected: Token, actual: anytype) !void {
            try std.testing.expectEqualDeep(LocToken{
                .col_nr = col_nr,
                .line_nr = line_nr,
                .token = expected,
            }, actual);
        }
    };
    const expectEqualIdxs = Fn.expectEqualIdxs;
    const expectEqualToken = Fn.expectEqualToken;

    const source =
        \\  foo bar
        \\ foo       bar
        \\ /*
        \\  *
        \\  */
        \\
    ;
    var lexer = Lexer.init(std.testing.allocator, source, "test.c");
    defer lexer.deinit();

    try expectEqualToken(1, 3, .{ .identifier = "foo" }, lexer.next());
    try expectEqualIdxs(1, 0, 2, 5, lexer);

    try expectEqualToken(1, 7, .{ .identifier = "bar" }, lexer.next());
    try expectEqualIdxs(1, 0, 6, 9, lexer);

    try expectEqualToken(2, 2, .{ .identifier = "foo" }, lexer.next());
    try expectEqualIdxs(2, 10, 11, 14, lexer);

    try expectEqualToken(2, 12, .{ .identifier = "bar" }, lexer.next());
    try expectEqualIdxs(2, 10, 21, 24, lexer);

    try expectEqualToken(3, 2, .{ .comment = "/*\n  *\n  */" }, lexer.next());
    try expectEqualIdxs(3, 25, 26, 37, lexer);
}
