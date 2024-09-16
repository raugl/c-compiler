const std = @import("std");
const clap = @import("clap");
const lex = @import("lexer.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    try args(alloc);

    // var lexer = try lex.FileLexer.init(alloc, "/home/raul/Downloads/git/dwm/dwm.c");
    var lexer = try lex.FileLexer.init(alloc, "/home/raul/dev/interpreter/src/example.c");
    defer lexer.deinit();

    const stdout = std.io.getStdOut().writer();
    while (try lexer.next()) |token| {
        try stdout.print("{}\n", .{token.token});

        // NOTE: Clean up the allocated string literals as we aren't yet using
        // the tokens for any other compilation stages.
        if (token.token == .literal_str) {
            switch (token.token.literal_str) {
                .utf8, .char => |str| alloc.free(str),
                .utf16 => |str| alloc.free(str),
                .utf32, .wchar => |str| alloc.free(str),
            }
        }
    }
    try stdout.writeAll("EoF\n");
}

fn args(alloc: std.mem.Allocator) !void {
    const help_msg =
        \\  -h, --help             Display this help and exit.
        \\  -o <str>...            Place the ouptup into <file>.
        \\  <str>...
        \\
    ;
    const params = comptime clap.parseParamsComptime(help_msg);

    var diag = clap.Diagnostic{};
    var opt = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = alloc,
    }) catch |err| {
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer opt.deinit();

    if (opt.args.help != 0) {
        std.debug.print(
            \\Usage: zcc [options] file...
            \\Options:
            \\{s}
            \\
        , .{help_msg});
        return;
    }
    if (opt.positionals.len != 0) {
        std.debug.print("input source files:", .{});
        for (opt.positionals) |file| {
            std.debug.print(" \"{s}\"", .{file});
        }
        std.debug.print("\n\n", .{});
    }
    for (opt.args.o) |file| {
        std.debug.print("output binary file: \"{s}\"\n", .{file});
    }
}
