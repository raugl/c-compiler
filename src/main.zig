// Overview of the stages of the compiler:
// [C File] -> plain text
//     -> [Lexer] -> tokens
//     -> [PreProcessor] -> tokens
//     -> [Parser] -> AST
//     -> [TypeChecker] -> nothing (previous AST)
//     -> [CodeGenerator] -> machine code
//     -> [Executable File]
//
// The `Lexer` takes in C source code in the form of plain text and categorizes
// sections of characters into `Token`s. The only form of error checking/reporting
// it does is towards the structure of a individual `Token` (syntax), and not to
// the higher level structure between distinct `Token`s i.e. "keyword.if" must be
// followed by "operator.left_paren" etc.
//
// The `PreProcessor` is analogous to the C preprocessor, takes in a stream of
// `Token`s from the `Lexer` and produces the final array of `Token`s that's sent
// to the `Parser`. These `Token`s are the result of all macro expansions in the
// provided source code. It reports errors related to the structure of preprocessor
// directives, and failure to `#include` files.
//
// The `Parser` takes in a slice of tokens an creates an Abstract Syntax Tree from
// them. An AST is a data structure that represents the structure of a program in
// higher level languages. It encodes all the possible statements and expressions
// of the language like ifs, loops, assignments, and the order they need to be
// executed in. Also function and type declarations. It can be used efficiently
// by other stages like type checkers, static analyzers, optimizers, code generators
// (lowering source code to machine code) etc. It reports issues related to the
// structure of these language constructs i.e. "can't define function inside other
// function", "unterminated parenthesis", "expected expression" etc.
//
// The `TypeChecker` takes in an Abstract Syntax Tree and verifies that the types
// of all the expressions in it are compatible with the places they are used in
// i.e. don't provide ints where strings are required, "you forgot to take the
// address of argument in function call" etc. It reports errors when types are
// incompatible, and it may provide hints for simple mistakes.
//
// The `CodeGenerator` takes in an Abstract Syntax Tree and translates it into
// `x86_64`/`amd64` machine code wrapped in either a `ELF` file for Linux/BSD,
// or a `PE` file for Windows.

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
