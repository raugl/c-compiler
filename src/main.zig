const std = @import("std");
const lex = @import("lexer.zig");

const demo_source =
    \\#pragma once
    \\#include "stdio.h"
    \\
    \\typedef struct {
    \\  float x, y;
    \\} vec2;
    \\int func(void) {}
    \\
    \\extern void printf(const char*, ...);
    \\
    \\int main(int argc, char** /*argv*/) {
    \\  const int num = 0xaf7'.E0uLL;
    \\  const int num = 0xULl;
    \\  /*const char ch = '\\';
    \\  printf("hello, %s", "world");
    \\}
;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    // var lexer = lex.Lexer.init(alloc, demo_source, "demo.c");
    var lexer = try lex.FileLexer.init(alloc, "/home/raul/Downloads/git/dwm/drw.h");
    defer lexer.deinit();

    const stdout = std.io.getStdOut().writer();
    while (try lexer.next()) |token| {
        try stdout.print("{}\n", .{token.token});
    }
    try stdout.writeAll("EoF\n");
}
