const std = @import("std");
const tok = @import("lexer.zig");

pub fn main() !void {
    const src =
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
        \\  const char ch = '\\\\';
        \\  printf("hello, %s", "world");
        \\}
    ;

    var iter = try tok.TokenIterator.init(src);
    while (try iter.next()) |token| {
        std.debug.print("{}\n", .{token});
    }
}
