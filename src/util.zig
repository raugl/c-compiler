const std = @import("std");

pub fn isLower(ch: u21) bool {
    return switch (ch) {
        'a'...'z' => true,
        else => false,
    };
}

pub fn isUpper(ch: u21) bool {
    return switch (ch) {
        'A'...'Z' => true,
        else => false,
    };
}

pub fn isNonZero(ch: u21) bool {
    return switch (ch) {
        '1'...'9' => true,
        else => false,
    };
}

pub fn isDigit(ch: u21) bool {
    return switch (ch) {
        '0'...'9' => true,
        else => false,
    };
}

pub fn isHex(ch: u21) bool {
    return switch (ch) {
        '0'...'9', 'A'...'F', 'a'...'f' => true,
        else => false,
    };
}

pub fn isBinary(ch: u21) bool {
    return ch == '0' or ch == '1';
}

pub fn isOctal(ch: u21) bool {
    return switch (ch) {
        '0'...'7' => true,
        else => false,
    };
}

pub fn isAlphabetic(ch: u21) bool {
    return switch (ch) {
        'A'...'Z', 'a'...'z' => true,
        else => false,
    };
}

pub fn isAlphanumeric(ch: u21) bool {
    return switch (ch) {
        '0'...'9', 'A'...'F', 'a'...'f' => true,
        else => false,
    };
}

pub fn isFloatSuffix(ch: u21) bool {
    return switch (ch) {
        'f', 'F', 'l', 'L' => true,
        else => false,
    };
}

pub fn isIntSuffix(ch: u21) bool {
    return switch (ch) {
        'u', 'U', 'l', 'L' => true,
        else => false,
    };
}

/// There are the symbol chars: ,<.>/?'";:|\{}[]+=-~()!@#$%^&*
pub fn isSymbol(ch: u21) bool {
    return switch (ch) {
        33...47, 58...64, 91...94, 123...126 => true,
        else => false,
    };
}

pub fn isLiteralBool(str: []const u8) bool {
    return std.mem.eql(u8, str, "true") or std.mem.eql(u8, str, "false");
}

pub fn isKeyword(str: []const u8) bool {
    const Fn = struct {
        fn compare(_: void, a: []const u8, b: []const u8) std.math.Order {
            return std.mem.order(u8, a, b);
        }

        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return compare({}, a, b) == .lt;
        }
    };

    const keywords = comptime blk: {
        var keywords = [_][]const u8{
            "auto",           "break",         "case",     "char",       "const",
            "continue",       "default",       "do",       "double",     "else",
            "enum",           "extern",        "float",    "for",        "goto",
            "if",             "inline",        "int",      "long",       "register",
            "restrict",       "return",        "short",    "signed",     "sizeof",
            "static",         "struct",        "switch",   "typedef",    "union",
            "unsigned",       "void",          "volatile", "while",      "_Alignas",
            "_Alignof",       "_Atomic",       "_Bool",    "_Complex",   "_Decimal128",
            "_Decimal32",     "_Decimal64",    "_Generic", "_Imaginary", "_Noreturn",
            "_Static_assert", "_Thread_local",
        };
        @setEvalBranchQuota(10_000);
        std.mem.sortUnstable([]const u8, &keywords, {}, Fn.lessThan);
        break :blk keywords;
    };

    return std.sort.binarySearch([]const u8, str, &keywords, {}, Fn.compare) != null;
}

pub fn parseInt(str: []const u8) !u128 {
    if (str.len == 0) return error.InvalidCharacter;

    var base = @as(u8, 10);
    var str_start = str[0..];
    if (str[0] == '0') {
        if (str.len > 1) switch (str[1]) {
            'b', 'B' => {
                base = 2;
                str_start = str[2..];
            },
            'x', 'X' => {
                base = 16;
                str_start = str[2..];
            },
            else => {
                base = 8;
                str_start = str[1..];
            },
        };
    }

    if (str_start[0] == '\'' or str_start[str_start.len - 1] == '\'') {
        return error.InvalidCharacter;
    }
    var acc = @as(u128, 0);
    for (str_start) |ch| {
        if (ch == '\'') continue;
        if (std.mem.indexOfScalar(u8, "uUlLzZ", ch) != null) break;

        const digit = try std.fmt.charToDigit(ch, base);
        acc = try std.math.mul(u128, acc, base);
        acc = try std.math.add(u128, acc, digit);
    }
    return acc;
}
