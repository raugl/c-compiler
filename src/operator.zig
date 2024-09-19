const std = @import("std");

pub const MAX_LEN = 3;
pub const Operator = enum {
    ampersand,
    assign,
    assign_add,
    assign_and,
    assign_div,
    assign_lsh,
    assign_mod,
    assign_mul,
    assign_or,
    assign_rsh,
    assign_sub,
    assign_xor,
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

pub const ParseResult = struct {
    op: Operator,
    len: u32,
};

/// Accepts any string and parses an operator from its beginning, eagerly (chooses
/// the longest operator it can find). It returns the parsed length, so you don't
/// need to specify the precise operator length when calling it.
pub fn parseOperator(str: []const u8) ?ParseResult {
    const OpData = struct {
        str: []const u8,
        op: Operator,
    };

    const Fn = struct {
        fn lessThan(_: void, lhs: OpData, rhs: OpData) bool {
            return std.mem.order(u8, lhs.str, rhs.str) == .lt;
        }

        fn compare(_: void, lhs: []const u8, rhs: OpData) std.math.Order {
            return std.mem.order(u8, lhs, rhs.str);
        }
    };

    const operators = comptime blk: {
        var operators = [_]OpData{
            .{ .str = "(", .op = .left_paren },
            .{ .str = ")", .op = .right_paren },
            .{ .str = "[", .op = .left_bracket },
            .{ .str = "]", .op = .right_bracket },
            .{ .str = "{", .op = .left_brace },
            .{ .str = "}", .op = .right_brace },
            .{ .str = "<", .op = .less },
            .{ .str = ",", .op = .comma },
            .{ .str = ">", .op = .greater },
            .{ .str = ".", .op = .period },
            .{ .str = ";", .op = .semicolon },
            .{ .str = ":", .op = .colon },
            .{ .str = "=", .op = .assign },
            .{ .str = "+", .op = .plus },
            .{ .str = "-", .op = .minus },
            .{ .str = "_", .op = .under_score },
            .{ .str = "/", .op = .slash },
            .{ .str = "?", .op = .question_mark },
            .{ .str = "\\", .op = .back_slash },
            .{ .str = "|", .op = .pipe },
            .{ .str = "~", .op = .tilde },
            .{ .str = "`", .op = .back_tick },
            .{ .str = "!", .op = .exclamation },
            .{ .str = "@", .op = .at_sign },
            .{ .str = "#", .op = .hash },
            .{ .str = "##", .op = .double_hash },
            .{ .str = "$", .op = .dollar },
            .{ .str = "%", .op = .percent },
            .{ .str = "^", .op = .carret },
            .{ .str = "&", .op = .ampersand },
            .{ .str = "*", .op = .asterisk },
            .{ .str = "++", .op = .double_plus },
            .{ .str = "--", .op = .double_minus },
            .{ .str = "||", .op = .logical_or },
            .{ .str = "&&", .op = .logical_and },
            .{ .str = "->", .op = .thin_arrow },
            .{ .str = ">>", .op = .right_shift },
            .{ .str = "<<", .op = .left_shift },
            .{ .str = "==", .op = .equal },
            .{ .str = "!=", .op = .not_equal },
            .{ .str = "<=", .op = .less_equal },
            .{ .str = ">=", .op = .greater_equal },
            .{ .str = "+=", .op = .assign_add },
            .{ .str = "-=", .op = .assign_sub },
            .{ .str = "*=", .op = .assign_mul },
            .{ .str = "/=", .op = .assign_div },
            .{ .str = "%=", .op = .assign_mod },
            .{ .str = "^=", .op = .assign_xor },
            .{ .str = "&=", .op = .assign_and },
            .{ .str = "|=", .op = .assign_or },
            .{ .str = ">>=", .op = .assign_rsh },
            .{ .str = "<<=", .op = .assign_lsh },
            .{ .str = "...", .op = .ellipses },
        };

        @setEvalBranchQuota(10_000);
        std.mem.sortUnstable(OpData, &operators, {}, Fn.lessThan);
        break :blk operators;
    };

    // PERF: This could be improved by splitting the different sized strings
    // into multiple sorted arrays, thus binary-searching over fewer items.
    var n = @min(str.len, MAX_LEN);
    while (n > 0) : (n -= 1) {
        if (std.sort.binarySearch(OpData, str[0..n], &operators, {}, Fn.compare)) |idx| {
            return ParseResult{ .op = operators[idx].op, .len = n };
        }
    }
    return null;
}

test "parseOperator" {
    const testing = std.testing;
    try testing.expectEqual(ParseResult{ .op = .thin_arrow, .len = 2 }, parseOperator("->"));
    try testing.expectEqual(ParseResult{ .op = .assign, .len = 1 }, parseOperator("=>"));
    try testing.expectEqual(ParseResult{ .op = .assign_add, .len = 2 }, parseOperator("+="));
    try testing.expectEqual(ParseResult{ .op = .double_plus, .len = 2 }, parseOperator("++"));
    try testing.expectEqual(ParseResult{ .op = .ellipses, .len = 3 }, parseOperator("..."));
    try testing.expectEqual(ParseResult{ .op = .plus, .len = 1 }, parseOperator("+"));
}

pub fn format(op: Operator) []const u8 {
    return switch (op) {
        .ampersand => "&",
        .assign => "=",
        .assign_add => "+=",
        .assign_and => "&=",
        .assign_div => "/=",
        .assign_lsh => "<<=",
        .assign_mod => "%=",
        .assign_mul => "*=",
        .assign_or => "|=",
        .assign_rsh => ">>=",
        .assign_sub => "-=",
        .assign_xor => "^=",
        .asterisk => "*",
        .at_sign => "@",
        .back_slash => "\\",
        .back_tick => "`",
        .carret => "^",
        .colon => ";",
        .comma => ",",
        .dollar => "$",
        .double_hash => "##",
        .double_minus => "--",
        .double_plus => "++",
        .ellipses => "...",
        .equal => "==",
        .exclamation => "!",
        .greater => ">",
        .greater_equal => ">=",
        .hash => "#",
        .left_brace => "{",
        .left_bracket => "[",
        .left_paren => "(",
        .left_shift => "<<",
        .less => "<",
        .less_equal => "<=",
        .logical_and => "&&",
        .logical_or => "||",
        .minus => "-",
        .not_equal => "!=",
        .percent => "%",
        .period => ".",
        .pipe => "|",
        .plus => "+",
        .question_mark => "?",
        .right_brace => "}",
        .right_bracket => "]",
        .right_paren => ")",
        .right_shift => ">>",
        .semicolon => ";",
        .slash => "/",
        .thin_arrow => "->",
        .tilde => "~",
        .under_score => "_",
    };
}
