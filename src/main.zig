const std = @import("std");
const tok = @import("new_lexer.zig");

pub fn main() !void {
    const source =
        \\#pragma once
        \\// #include "stdio.h"
        \\
        \\typedef struct {
        \\  float x, y;
        \\} vec2;
        \\ int func(void) {}
        \\
        \\extern void printf(const char*, ...);
        \\
        \\int main(int argc, char** argv) {
        \\  const int num = 0xaF7'0uLL;
        \\  const char ch = '\\';
        \\  printf("hello, %s", "world");
        \\}
    ;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    var lexer = tok.Lexer.init(alloc, source, "demo.c");
    defer lexer.deinit();
    const stdout = std.io.getStdOut().writer();
    while (try lexer.next()) |token| {
        try stdout.print("{}\n", .{token.token});
    }
    try stdout.writeAll("EoF\n");
}
