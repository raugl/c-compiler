const std = @import("std");

pub const Keyword = enum {
    alignas,
    alignof,
    auto,
    bool,
    break_,
    case,
    char,
    const_,
    constexpr,
    continue_,
    default,
    do,
    double,
    else_,
    enum_,
    extern_,
    false,
    float,
    for_,
    goto,
    if_,
    inline_,
    int,
    long,
    nullptr,
    register,
    restrict,
    return_,
    short,
    signed,
    sizeof,
    static,
    static_assert,
    struct_,
    switch_,
    thread_local,
    true,
    typedef,
    typeof,
    typeof_unqual,
    union_,
    unsigned,
    void,
    volatile_,
    while_,
    _Alignas,
    _Alignof,
    _Atomic,
    _BitInt,
    _Bool,
    _Complex,
    _Decimal32,
    _Decimal64,
    _Decimal128,
    _Generic,
    _Imaginary,
    _Noreturn,
    _Static_assert,
    _Thread_local,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = .{ fmt, options };

        const str = switch (self) {
            .alignas => "alignas",
            .alignof => "alignof",
            .auto => "auto",
            .bool => "bool",
            .break_ => "break",
            .case => "case",
            .char => "char",
            .const_ => "const",
            .constexpr => "constexpr",
            .continue_ => "continue",
            .default => "default",
            .do => "do",
            .double => "double",
            .else_ => "else",
            .enum_ => "enum",
            .extern_ => "extern",
            .false => "false",
            .float => "float",
            .for_ => "for",
            .goto => "goto",
            .if_ => "if",
            .inline_ => "inline",
            .int => "int",
            .long => "long",
            .nullptr => "nullptr",
            .register => "register",
            .restrict => "restrict",
            .return_ => "return",
            .short => "short",
            .signed => "signed",
            .sizeof => "sizeof",
            .static => "static",
            .static_assert => "static_assert",
            .struct_ => "struct",
            .switch_ => "switch",
            .thread_local => "thread_local",
            .true => "true",
            .typedef => "typedef",
            .typeof => "typeof",
            .typeof_unqual => "typeof_unqual",
            .union_ => "union",
            .unsigned => "unsigned",
            .void => "void",
            .volatile_ => "volatile",
            .while_ => "while",
            ._Alignas => "_Alignas",
            ._Alignof => "_Alignof",
            ._Atomic => "_Atomic",
            ._BitInt => "_BitInt",
            ._Bool => "_Bool",
            ._Complex => "_Complex",
            ._Decimal32 => "_Decimal32",
            ._Decimal64 => "_Decimal64",
            ._Decimal128 => "_Decimal128",
            ._Generic => "_Generic",
            ._Imaginary => "_Imaginary",
            ._Noreturn => "_Noreturn",
            ._Static_assert => "_Static_assert",
            ._Thread_local => "_Thread_local",
        };
        try writer.writeAll(str);
    }
};

// FIXME: This description is no longer true
/// Accepts any string and parses a keyword from its beginning up to however long
/// the potential keyword is. It returns the parsed length, so you don't need to
/// specify the precise keyword length when calling it.
pub fn parseKeyword(str: []const u8) ?Keyword {
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
            .{ .str = "alignas", .kw = .alignas },
            .{ .str = "alignof", .kw = .alignof },
            .{ .str = "auto", .kw = .auto },
            .{ .str = "bool", .kw = .bool },
            .{ .str = "break", .kw = .break_ },
            .{ .str = "case", .kw = .case },
            .{ .str = "char", .kw = .char },
            .{ .str = "const", .kw = .const_ },
            .{ .str = "constexpr", .kw = .constexpr },
            .{ .str = "continue", .kw = .continue_ },
            .{ .str = "default", .kw = .default },
            .{ .str = "do", .kw = .do },
            .{ .str = "double", .kw = .double },
            .{ .str = "else", .kw = .else_ },
            .{ .str = "enum", .kw = .enum_ },
            .{ .str = "extern", .kw = .extern_ },
            .{ .str = "false", .kw = .false },
            .{ .str = "float", .kw = .float },
            .{ .str = "for", .kw = .for_ },
            .{ .str = "goto", .kw = .goto },
            .{ .str = "if", .kw = .if_ },
            .{ .str = "inline", .kw = .inline_ },
            .{ .str = "int", .kw = .int },
            .{ .str = "long", .kw = .long },
            .{ .str = "nullptr", .kw = .nullptr },
            .{ .str = "register", .kw = .register },
            .{ .str = "restrict", .kw = .restrict },
            .{ .str = "return", .kw = .return_ },
            .{ .str = "short", .kw = .short },
            .{ .str = "signed", .kw = .signed },
            .{ .str = "sizeof", .kw = .sizeof },
            .{ .str = "static", .kw = .static },
            .{ .str = "static_assert", .kw = .static_assert },
            .{ .str = "struct", .kw = .struct_ },
            .{ .str = "switch", .kw = .switch_ },
            .{ .str = "thread_local", .kw = .thread_local },
            .{ .str = "true", .kw = .true },
            .{ .str = "typedef", .kw = .typedef },
            .{ .str = "typeof", .kw = .typeof },
            .{ .str = "typeof_unqual", .kw = .typeof_unqual },
            .{ .str = "union", .kw = .union_ },
            .{ .str = "unsigned", .kw = .unsigned },
            .{ .str = "void", .kw = .void },
            .{ .str = "volatile", .kw = .volatile_ },
            .{ .str = "while", .kw = .while_ },
            .{ .str = "_Alignas", .kw = ._Alignas },
            .{ .str = "_Alignof", .kw = ._Alignof },
            .{ .str = "_Atomic", .kw = ._Atomic },
            .{ .str = "_BitInt", .kw = ._BitInt },
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
        return keywords[idx].kw;
    }
    return null;
}

test "parseKeyword" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(.if_, parseKeyword("if"));
    try expectEqual(.int, parseKeyword("int"));
    try expectEqual(.inline_, parseKeyword("inline"));
    try expectEqual(._Decimal64, parseKeyword("_Decimal64"));
    try expectEqual(._Decimal128, parseKeyword("_Decimal128"));
}
