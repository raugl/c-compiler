const std = @import("std");
const kw = @import("keyword.zig");
const op = @import("operator.zig");
const Directive = @import("preproc.zig").Directive;

/// Wrapper around `Token` that include source location information
pub const LocToken = struct {
    token: Token,
    line_nr: u16 = 0,
    col_nr: u16 = 0,
};

pub const Token = union(enum) {
    const Self = @This();

    comment: []const u8,
    identifier: []const u8,
    literal_bool: bool,
    literal_int: struct {
        value: u128,
        width: u8,
        signed: bool,
    },
    literal_float: struct {
        value: f128,
        width: u8,
    },
    literal_str: union(enum) {
        char: []const u8,
        utf8: []const u8,
        utf16: []const u16,
        utf32: []const u32,
        wchar: []const u32, // FIXME: For whatever-the-fuck reason `wchar_t` on Windows is only 16 bits wide
    },
    keyword: kw.Keyword,
    operator: op.Operator,
    preproc: Directive,

    // BUG: This crashes if the printed unicode codepoint is invalid. It would also be nice to re-escape special chars
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = .{ fmt, options };
        var codepoint_buf = [_]u8{undefined} ** 4;

        switch (self) {
            .literal_bool => |state| try writer.print("literal bool: {}", .{state}),
            .literal_float => |float| try writer.print(
                "literal f{}: {}",
                .{ float.width, float.value },
            ),
            .literal_int => |data| {
                try writer.print("literal {c}{}: {} ('{c}')", .{
                    @as(u8, if (data.signed) 'i' else 'u'),
                    data.width,
                    data.value,
                    @as(u8, @truncate(data.value)),
                });
            },
            .literal_str => |lit_str| switch (lit_str) {
                .char => |str| try writer.print("literal str: \"{s}\"", .{str}),
                .utf8 => |str| try writer.print("literal str: u8\"{s}\"", .{str}),
                .utf16 => |str| {
                    // try writer.print("literal str: u\"{any}\"", .{str});
                    try writer.writeAll("literal str: u\"");
                    var it = std.unicode.Utf16LeIterator.init(str);
                    while (try it.nextCodepoint()) |codepoint| {
                        const len = try std.unicode.utf8Encode(codepoint, &codepoint_buf);
                        try writer.writeAll(codepoint_buf[0..len]);
                    }
                    try writer.writeAll("\"");
                },
                .utf32 => |str| {
                    try writer.writeAll("literal str: U\"");
                    for (str) |ch| {
                        const codepoint: u21 = @truncate(@min(ch, 0x10_ffff));
                        const len = try std.unicode.utf8Encode(codepoint, &codepoint_buf);
                        try writer.writeAll(codepoint_buf[0..len]);
                    }
                    try writer.writeAll("\"");
                },
                .wchar => |str| { // FIXME: For whatever-the-fuck reason `wchar_t` on Windows is only 16 bits wide
                    try writer.writeAll("literal str: L\"");
                    for (str) |ch| {
                        const codepoint: u21 = @truncate(@min(ch, 0x10_ffff));
                        const len = try std.unicode.utf8Encode(codepoint, &codepoint_buf);
                        try writer.writeAll(codepoint_buf[0..len]);
                    }
                    try writer.writeAll("\"");
                },
            },
            .comment => |body| try writer.print("comment: {s}", .{body}),
            .identifier => |name| try writer.print("identifier: \"{s}\"", .{name}),
            .keyword => |keyword| try writer.print("keyword.{s}", .{kw.format(keyword)}),
            .operator => |operator| try writer.print("operator: \"{s}\"", .{op.format(operator)}),
            .preproc => |preproc| try writer.print("preproc.{}", .{preproc}),
        }
    }
};

pub const TokenTag = enum {
    identifier,
    comment,

    // Literals
    literal_str,
    literal_int,
    literal_float,
    literal_true,
    literal_false,

    // PreProc
    preproc_include,
    preproc_pragma,
    preproc_define,
    preproc_undef,
    preproc_ifndef,
    preproc_ifdef,
    preproc_if,
    preproc_else,
    preproc_elif,
    preproc_elifdef,
    preproc_elifndef,
    preproc_endif,

    // Keywords
    keyword_auto,
    keyword_break,
    keyword_case,
    keyword_char,
    keyword_const,
    keyword_continue,
    keyword_default,
    keyword_do,
    keyword_double,
    keyword_else,
    keyword_enum,
    keyword_extern,
    keyword_float,
    keyword_for,
    keyword_goto,
    keyword_if,
    keyword_inline,
    keyword_int,
    keyword_long,
    keyword_register,
    keyword_restrict,
    keyword_return,
    keyword_short,
    keyword_signed,
    keyword_sizeof,
    keyword_static,
    keyword_struct,
    keyword_switch,
    keyword_typedef,
    keyword_union,
    keyword_unsigned,
    keyword_void,
    keyword_volatile,
    keyword_while,
    keyword_Alignas,
    keyword_Alignof,
    keyword_Atomic,
    keyword_Bool,
    keyword_Complex,
    keyword_Decimal128,
    keyword_Decimal32,
    keyword_Decimal64,
    keyword_Generic,
    keyword_Imaginary,
    keyword_Noreturn,
    keyword_Static_assert,
    keyword_Thread_local,

    // Operators
    ampersand,
    assign,
    assign_add,
    assign_div,
    assign_mod,
    assign_mul,
    assign_sub,
    asterisk,
    at_sign,
    back_slash,
    back_tick,
    carret,
    colon,
    comma,
    dollar,
    double_hash,
    double_minus,
    double_plus,
    ellipses,
    equal,
    exclamation,
    greater,
    greater_equal,
    hash,
    left_brace,
    left_bracket,
    left_paren,
    left_shift,
    less,
    less_equal,
    logical_and,
    logical_or,
    minus,
    not_equal,
    percent,
    period,
    pipe,
    plus,
    question_mark,
    right_brace,
    right_bracket,
    right_paren,
    right_shift,
    semicolon,
    slash,
    thin_arrow,
    tilde,
    under_score,
};

pub fn eql(token: Token, tag: TokenTag) bool {
    return switch (token) {
        .comment => (tag == .comment),
        .identifier => (tag == .identifier),
        .literal_str => (tag == .literal_str),
        .literal_int => (tag == .literal_int),
        .literal_float => (tag == .literal_float),
        .literal_bool => (tag == .literal_false or tag == .literal_true),
        .preproc => switch (tag) {
            .preproc_include,
            .preproc_pragma,
            .preproc_define,
            .preproc_undef,
            .preproc_ifndef,
            .preproc_ifdef,
            .preproc_if,
            .preproc_else,
            .preproc_elif,
            .preproc_elifdef,
            .preproc_elifndef,
            .preproc_endif,
            => return true,
            else => return false,
        },
        .keyword => switch (tag) {
            .keyword_auto,
            .keyword_break,
            .keyword_case,
            .keyword_char,
            .keyword_const,
            .keyword_continue,
            .keyword_default,
            .keyword_do,
            .keyword_double,
            .keyword_else,
            .keyword_enum,
            .keyword_extern,
            .keyword_float,
            .keyword_for,
            .keyword_goto,
            .keyword_if,
            .keyword_inline,
            .keyword_int,
            .keyword_long,
            .keyword_register,
            .keyword_restrict,
            .keyword_return,
            .keyword_short,
            .keyword_signed,
            .keyword_sizeof,
            .keyword_static,
            .keyword_struct,
            .keyword_switch,
            .keyword_typedef,
            .keyword_union,
            .keyword_unsigned,
            .keyword_void,
            .keyword_volatile,
            .keyword_while,
            .keyword_Alignas,
            .keyword_Alignof,
            .keyword_Atomic,
            .keyword_Bool,
            .keyword_Complex,
            .keyword_Decimal128,
            .keyword_Decimal32,
            .keyword_Decimal64,
            .keyword_Generic,
            .keyword_Imaginary,
            .keyword_Noreturn,
            .keyword_Static_assert,
            .keyword_Thread_local,
            => return true,
            else => return false,
        },
        .operator => switch (tag) {
            .ampersand,
            .assign,
            .assign_add,
            .assign_div,
            .assign_mod,
            .assign_mul,
            .assign_sub,
            .asterisk,
            .at_sign,
            .back_slash,
            .back_tick,
            .carret,
            .colon,
            .comma,
            .dollar,
            .double_hash,
            .double_minus,
            .double_plus,
            .ellipses,
            .equal,
            .exclamation,
            .greater,
            .greater_equal,
            .hash,
            .left_brace,
            .left_bracket,
            .left_paren,
            .left_shift,
            .less,
            .less_equal,
            .logical_and,
            .logical_or,
            .minus,
            .not_equal,
            .percent,
            .period,
            .pipe,
            .plus,
            .question_mark,
            .right_brace,
            .right_bracket,
            .right_paren,
            .right_shift,
            .semicolon,
            .slash,
            .thin_arrow,
            .tilde,
            .under_score,
            => return true,
            else => return false,
        },
    };
}
