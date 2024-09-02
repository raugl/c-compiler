const std = @import("std");
const tok = @import("new_lexer.zig");

pub fn main() !void {
    // const src =
    //     \\#pragma once
    //     \\// #include "stdio.h"
    //     \\
    //     \\typedef struct {
    //     \\  float x, y;
    //     \\} vec2;
    //     \\ int func(void) {}
    //     \\
    //     \\extern void printf(const char*, ...);
    //     \\
    //     \\int main(int argc, char** argv) {
    //     \\  const int num = 0xaF7'0uLL;
    //     \\  const char ch = '\\\\';
    //     \\  printf("hello, %s", "world");
    //     \\}
    // ;
    //
    // var iter = try tok.TokenIterator.init(src);
    // while (try iter.next()) |token| {
    //     std.debug.print("{}\n", .{token});
    // }

    const source =
        \\1e10
        \\1e-5L
        \\1.
        \\1.e-2
        \\3.14
        \\.1f
        \\0.1e-1L
        \\0x1ffp10
        \\0X0p-1
        \\0x1.p0
        \\0xf.p-1
        \\0x0.123p-1
        \\0xa.bp10l
    ;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    var lexer = tok.Lexer.init(alloc, source);
    defer lexer.deinit();
    while (try lexer.next()) |token| {
        std.debug.print("{}\n", .{token});
    }
    std.debug.print("EoF\n", .{});
}
