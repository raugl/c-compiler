const std = @import("std");

pub const Keyword = enum {
    auto,
    break_,
    case,
    char,
    const_,
    continue_,
    default,
    do,
    double,
    else_,
    enum_,
    extern_,
    float,
    for_,
    goto,
    if_,
    inline_,
    int,
    long,
    register,
    restrict,
    return_,
    short,
    signed,
    sizeof,
    static,
    struct_,
    switch_,
    typedef,
    union_,
    unsigned,
    void,
    volatile_,
    while_,
    _Alignas,
    _Alignof,
    _Atomic,
    _Bool,
    _Complex,
    _Decimal128,
    _Decimal32,
    _Decimal64,
    _Generic,
    _Imaginary,
    _Noreturn,
    _Static_assert,
    _Thread_local,
};

pub const ParseResult = struct {
    kw: Keyword,
    len: u32,
};

/// Accepts any string and parses a keyword from its beginning up to however long
/// the potential keyword is. It returns the parsed length, so you don't need to
/// specify the precise keyword length when calling it.
pub fn parseKeyword(str: []const u8) ?ParseResult {
    const KeywordData = struct {
        str: []const u8,
        kw: Keyword,
    };

    const Fn = struct {
        fn lessThan(_: void, lhs: KeywordData, rhs: KeywordData) bool {
            return std.mem.order(u8, lhs.str, rhs.str) == .lt;
        }

        fn compare(_: void, lhs: []const u8, rhs: KeywordData) std.math.Order {
            return std.mem.order(u8, lhs, rhs.str);
        }
    };

    const keywords = comptime blk: {
        var keywords = [_]KeywordData{
            .{ .str = "auto", .kw = .auto },
            .{ .str = "break", .kw = .break_ },
            .{ .str = "case", .kw = .case },
            .{ .str = "char", .kw = .char },
            .{ .str = "const", .kw = .const_ },
            .{ .str = "continue", .kw = .continue_ },
            .{ .str = "default", .kw = .default },
            .{ .str = "do", .kw = .do },
            .{ .str = "double", .kw = .double },
            .{ .str = "else", .kw = .else_ },
            .{ .str = "enum", .kw = .enum_ },
            .{ .str = "extern", .kw = .extern_ },
            .{ .str = "float", .kw = .float },
            .{ .str = "for", .kw = .for_ },
            .{ .str = "goto", .kw = .goto },
            .{ .str = "if", .kw = .if_ },
            .{ .str = "inline", .kw = .inline_ },
            .{ .str = "int", .kw = .int },
            .{ .str = "long", .kw = .long },
            .{ .str = "register", .kw = .register },
            .{ .str = "restrict", .kw = .restrict },
            .{ .str = "return", .kw = .return_ },
            .{ .str = "short", .kw = .short },
            .{ .str = "signed", .kw = .signed },
            .{ .str = "sizeof", .kw = .sizeof },
            .{ .str = "static", .kw = .static },
            .{ .str = "struct", .kw = .struct_ },
            .{ .str = "switch", .kw = .switch_ },
            .{ .str = "typedef", .kw = .typedef },
            .{ .str = "union", .kw = .union_ },
            .{ .str = "unsigned", .kw = .unsigned },
            .{ .str = "void", .kw = .void },
            .{ .str = "volatile", .kw = .volatile_ },
            .{ .str = "while", .kw = .while_ },
            .{ .str = "_Alignas", .kw = ._Alignas },
            .{ .str = "_Alignof", .kw = ._Alignof },
            .{ .str = "_Atomic", .kw = ._Atomic },
            .{ .str = "_Bool", .kw = ._Bool },
            .{ .str = "_Complex", .kw = ._Complex },
            .{ .str = "_Decimal32", .kw = ._Decimal32 },
            .{ .str = "_Decimal64", .kw = ._Decimal64 },
            .{ .str = "_Decimal128", .kw = ._Decimal128 },
            .{ .str = "_Generic", .kw = ._Generic },
            .{ .str = "_Imaginary", .kw = ._Imaginary },
            .{ .str = "_Noreturn", .kw = ._Noreturn },
            .{ .str = "_Static_assert", .kw = ._Static_assert },
            .{ .str = "_Thread_local", .kw = ._Thread_local },
        };

        @setEvalBranchQuota(10_000);
        std.mem.sortUnstable(KeywordData, &keywords, {}, Fn.lessThan);
        break :blk keywords;
    };

    if (std.sort.binarySearch(KeywordData, str, &keywords, {}, Fn.compare)) |idx| {
        return ParseResult{
            .kw = keywords[idx].kw,
            .len = @intCast(keywords[idx].str.len),
        };
    }
    return null;
}

test "parseKeyword" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(ParseResult{ .kw = .if_, .len = 2 }, parseKeyword("if"));
    try expectEqual(ParseResult{ .kw = .int, .len = 3 }, parseKeyword("int"));
    try expectEqual(ParseResult{ .kw = .inline_, .len = 6 }, parseKeyword("inline"));
    try expectEqual(ParseResult{ .kw = ._Decimal64, .len = 10 }, parseKeyword("_Decimal64"));
    try expectEqual(ParseResult{ .kw = ._Decimal128, .len = 11 }, parseKeyword("_Decimal128"));
}

pub fn format(keyword: Keyword) []const u8 {
    return switch (keyword) {
        .auto => "auto",
        .break_ => "break",
        .case => "case",
        .char => "char",
        .const_ => "const",
        .continue_ => "continue",
        .default => "default",
        .do => "do",
        .double => "double",
        .else_ => "else",
        .enum_ => "enum",
        .extern_ => "extern",
        .float => "float",
        .for_ => "for",
        .goto => "goto",
        .if_ => "if",
        .inline_ => "inline",
        .int => "int",
        .long => "long",
        .register => "register",
        .restrict => "restrict",
        .return_ => "return",
        .short => "short",
        .signed => "signed",
        .sizeof => "sizeof",
        .static => "static",
        .struct_ => "struct",
        .switch_ => "switch",
        .typedef => "typedef",
        .union_ => "union",
        .unsigned => "unsigned",
        .void => "void",
        .volatile_ => "volatile",
        .while_ => "while",
        ._Alignas => "_Alignas",
        ._Alignof => "_Alignof",
        ._Atomic => "_Atomic",
        ._Bool => "_Bool",
        ._Complex => "_Complex",
        ._Decimal128 => "_Decimal128",
        ._Decimal32 => "_Decimal32",
        ._Decimal64 => "_Decimal64",
        ._Generic => "_Generic",
        ._Imaginary => "_Imaginary",
        ._Noreturn => "_Noreturn",
        ._Static_assert => "_Static_assert",
        ._Thread_local => "_Thread_local",
    };
}
