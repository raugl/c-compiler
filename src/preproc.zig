const std = @import("std");

pub const Directive = enum {
    define,
    undef,
    include,
    if_,
    ifdef,
    ifndef,
    else_,
    elif,
    elifdef,
    elifndef,
    endif,
    line,
    embed,
    error_,
    warning,
    pragma,
    empty,
    end,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = .{ fmt, options };

        const str = switch (self) {
            .define => "#define",
            .undef => "#undef",
            .include => "#include",
            .if_ => "#if",
            .ifdef => "#ifdef",
            .ifndef => "#ifndef",
            .else_ => "#else",
            .elif => "#elif",
            .elifdef => "#elifdef",
            .elifndef => "#elifndef",
            .endif => "#endif",
            .line => "#line",
            .embed => "#embed",
            .error_ => "#error",
            .warning => "#warning",
            .pragma => "#pragma",
            .empty => "#",
            .end => "end",
        };
        try writer.writeAll(str);
    }
};

pub fn parseDirective(str: []const u8) ?Directive {
    const DirectiveData = struct {
        str: []const u8,
        dir: Directive,
    };

    const Fn = struct {
        fn lessThan(_: void, lhs: DirectiveData, rhs: DirectiveData) bool {
            return std.mem.order(u8, lhs.str, rhs.str) == .lt;
        }

        fn compare(_: void, lhs: []const u8, rhs: DirectiveData) std.math.Order {
            return std.mem.order(u8, lhs, rhs.str);
        }
    };

    const directives = comptime blk: {
        var directives = [_]DirectiveData{
            .{ .str = "define", .dir = .define },
            .{ .str = "undef", .dir = .undef },
            .{ .str = "include", .dir = .include },
            .{ .str = "if", .dir = .if_ },
            .{ .str = "ifdef", .dir = .ifdef },
            .{ .str = "ifndef", .dir = .ifndef },
            .{ .str = "else", .dir = .else_ },
            .{ .str = "elif", .dir = .elif },
            .{ .str = "elifdef", .dir = .elifdef },
            .{ .str = "elifndef", .dir = .elifndef },
            .{ .str = "endif", .dir = .endif },
            .{ .str = "line", .dir = .line },
            .{ .str = "embed", .dir = .embed },
            .{ .str = "error", .dir = .error_ },
            .{ .str = "warning", .dir = .warning },
            .{ .str = "pragma", .dir = .pragma },
        };

        @setEvalBranchQuota(10_000);
        std.mem.sortUnstable(DirectiveData, &directives, {}, Fn.lessThan);
        break :blk directives;
    };

    if (std.sort.binarySearch(DirectiveData, str, &directives, {}, Fn.compare)) |idx| {
        return directives[idx].dir;
    }
    return null;
}
