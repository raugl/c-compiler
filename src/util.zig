const std = @import("std");

pub fn decimal(str: []const u8) ?usize {
    if (str.len > 0 and '0' <= str[0] and str[0] <= '9') {
        return 1;
    } else return null;
}

pub fn nonZero(str: []const u8) ?usize {
    if (str.len > 0) switch (str[0]) {
        '1'...'9' => return 1,
        else => return null,
    } else return null;
}

pub fn binary(str: []const u8) ?usize {
    if (str.len > 0) switch (str[0]) {
        '0', '1' => return 1,
        else => return null,
    } else return null;
}

pub fn octal(str: []const u8) ?usize {
    if (str.len > 0) switch (str[0]) {
        '0'...'7' => return 1,
        else => return null,
    } else return null;
}

pub fn hex(str: []const u8) ?usize {
    if (str.len > 0) switch (str[0]) {
        '0'...'9', 'a'...'f', 'A'...'F' => return 1,
        else => return null,
    } else return null;
}

pub fn whitespace(str: []const u8) ?usize {
    return std.mem.indexOfNone(u8, str, &std.ascii.whitespace);
}

pub fn alphabetic(str: []const u8) ?usize {
    if (str.len > 0) switch (str[0]) {
        'a'...'z', 'A'...'Z' => return 1,
        else => return null,
    } else return null;
}

pub fn alphaNumeric(str: []const u8) ?usize {
    if (str.len > 0) switch (str[0]) {
        '0'...'9', 'a'...'z', 'A'...'Z' => return 1,
        else => return null,
    } else return null;
}

pub fn parseInt(str: []const u8) !u128 {
    if (str.len == 0) return error.InvalidCharacter;

    const base: u8, var str_start = blk: {
        if (str[0] == '0') {
            if (str.len >= 2) break :blk switch (str[1]) {
                'x', 'X' => .{ 16, str[2..] },
                'b', 'B' => .{ 2, str[2..] },
                else => .{ 8, str[1..] },
            };
        }
        break :blk .{ 10, str };
    };

    var num = @as(u128, 0);
    _ = try parseIntBase(u128, &str_start, &num, base);
    return num;
}

// TODO: This implementation is probably suboptimal
pub fn parseFloat(str: []const u8) !f128 {
    if (str.len == 0) return error.InvalidCharacter;

    var base: u8, var str_start = blk: {
        const is_hex = str.len >= 2 and str[0] == '0' and str[1] == 'x' or str[1] == 'X';
        break :blk if (is_hex) .{ 16, str[2..] } else .{ 10, str };
    };

    var mantissa = @as(u128, 0);
    if (str_start.len > 0 and isBase(str_start[0], base)) {
        _ = try parseIntBase(u128, &str_start, &mantissa, base);
    }
    if (str_start.len > 0 and str_start[0] == '.') {
        str_start = str_start[1..];
    }
    var exponent = @as(i64, 0);
    if (str_start.len > 0 and isBase(str_start[0], base)) {
        const num_digits = try parseIntBase(u128, &str_start, &mantissa, base);
        exponent = -@as(i64, @intCast(num_digits));
        if (base == 16) exponent *= 4;
    }

    if (str_start.len > 0 and std.mem.indexOfScalar(u8, "eEpP", str_start[0]) != null) {
        str_start = str_start[1..];

        var negative = false;
        if (str_start.len > 0 and str_start[0] == '-' or str_start[0] == '+') {
            if (str_start[0] == '-') negative = true;
            str_start = str_start[1..];
        }

        var real_exponent = @as(i64, 0);
        _ = try parseIntBase(i64, &str_start, &real_exponent, 10);
        switch (negative) {
            true => exponent -= real_exponent,
            false => exponent += real_exponent,
        }
    }

    if (base == 16) base = 2;
    return @as(f128, @floatFromInt(mantissa)) * std.math.pow(f64, @floatFromInt(base), @floatFromInt(exponent));
}

fn isBase(ch: u8, base: u8) bool {
    const value = switch (ch) {
        '0'...'9' => ch - '0',
        'A'...'Z' => ch - 'A' + 10,
        'a'...'z' => ch - 'a' + 10,
        else => return false,
    };
    return value < base;
}

fn parseIntBase(comptime T: type, str: *[]const u8, num: *T, base: u8) !u16 {
    if (str.len == 0) return 0;
    if (str.*[0] == '\'') return error.InvalidCharacter;

    var i = @as(u16, 0);
    var num_digits = @as(u16, 0);
    while (i < str.len) : (i += 1) {
        if (str.*[i] == '\'') continue;
        const digit = std.fmt.charToDigit(str.*[i], base) catch break;

        num.* *%= base;
        num.* +%= digit;
        num_digits += 1;
    }

    if (str.*[i - 1] == '\'') return error.InvalidCharacter;
    str.* = str.*[i..];
    return num_digits;
}

// Function taken from https://github.com/fig-eater/zig-function-overloading/blob/1.0.3/src/overloading.zig
pub fn isConvertibleTo(comptime From: type, comptime To: type) bool {
    return comptime return_block: {
        if (From == To) break :return_block true;
        const from_type_info = @typeInfo(From);
        const to_type_info = @typeInfo(To);

        break :return_block switch (to_type_info) {
            .Optional => |to| {
                if (isConvertibleTo(From, to.child)) break :return_block true;

                // check if converting c-pointer to optional pointer
                if (from_type_info == .Pointer and from_type_info.Pointer.is_allowzero) {
                    break :return_block isPointerConvertibleTo(From, to.child, false, false);
                }
                break :return_block false;
            },
            .ComptimeInt, .ComptimeFloat => To == From, // this should be handled above From == To
            .Int => |to| switch (from_type_info) {
                .Int => |from| from.bits == to.bits and
                    from.signedness == to.signedness,
                .ComptimeInt => true,
                else => false,
            },
            .Float => |to| switch (from_type_info) {
                .Float => |from| from.bits == to.bits,
                .ComptimeFloat => true,
                else => false,
            },
            .Pointer => |_| switch (from_type_info) {
                // BAD BAD BAD
                // .Array => |from| {
                //     if (!isConvertibleTo(from.child, to.child)) break :return_block false;
                //     break :return_block true;
                // },
                .Pointer => isPointerConvertibleTo(From, To, true, false),
                .Optional => |from| if (@typeInfo(from.child) == .Pointer)
                    isPointerConvertibleTo(from.child, To, false, true)
                else
                    false,
                else => false,
            },
            // .Array => |to| switch (from_type_info) {
            //     .Array => |from| from.
            // },
            .ErrorUnion => |to| isConvertibleTo(From, to.error_set) or
                isConvertibleTo(From, to.payload),
            else => false,
        };
    };
}

// Function taken from https://github.com/fig-eater/zig-function-overloading/blob/1.0.3/src/overloading.zig
fn isPointerConvertibleTo(
    comptime From: type,
    comptime To: type,
    comptime check_allowzero_match: bool,
    comptime only_allow_to_c_ptr: bool,
) bool {
    return comptime return_block: {
        const from = @typeInfo(From).Pointer;
        const to = @typeInfo(To).Pointer;

        const from_child_type_info = @typeInfo(from.child);
        const from_array_ptr_to_slice = from_child_type_info == .Array and
            isConvertibleTo(from_child_type_info.Array.child, to.child);

        // if FROM is const, make sure TO is const
        if (from.is_const and !to.is_const) break :return_block false;

        // if TO is expected to be volatile make sure FROM is volatile
        if (to.is_volatile and !from.is_volatile) break :return_block false;

        // make sure alignment matches
        if (to.alignment != from.alignment) break :return_block false;

        // make sure address space matches
        if (to.address_space != from.address_space) break :return_block false;

        // if FROM can allow zero, make sure TO can allow zero
        if (check_allowzero_match and (from.is_allowzero and !to.is_allowzero))
            break :return_block false;

        // if assigning to a pointer with a sentinel
        // make sure from has a matching sentinel
        // otherwise, they are not convertible
        if (to.sentinel) |to_sentinel| {
            if (if (from_array_ptr_to_slice)
                from_child_type_info.Array.sentinel
            else
                from.sentinel) |from_sentinel|
            {
                if (!@import("std").mem.eql(
                    to.child,
                    @as(*const to.child, @ptrCast(@alignCast(from_sentinel)))[0..1],
                    @as(*const to.child, @ptrCast(@alignCast(to_sentinel)))[0..1],
                )) break :return_block false;
            } else break :return_block false;
        }

        if (!from_array_ptr_to_slice) {
            if (only_allow_to_c_ptr) {
                if (to.size != .C) break :return_block false;
                switch (from.size) {
                    .One => {},
                    .Many => {},
                    .Slice => break :return_block false,
                    .C => {},
                }
            } else if (!switch (from.size) {
                .One => switch (to.size) {
                    .One => true,
                    .Many => false,
                    .Slice => false,
                    .C => true,
                },
                .Many => switch (to.size) {
                    .One => false,
                    .Many => true,
                    .Slice => false,
                    .C => true,
                },
                .Slice => switch (to.size) {
                    .One => false,
                    // slice to many is only convertible they both have matching
                    // sentinels. the matching sentinel check is done above. so if
                    // to has a sentinel then from has a matching sentinel.
                    .Many => to.sentinel != null,
                    .Slice => true,
                    .C => false,
                },
                .C => switch (to.size) {
                    // although C pointers ARE convertible to single pointers and
                    // many pointers,
                    // if they are null, they will cause an error when converting
                    // so we won't allow this by default, only when check_allowzero_match is false.
                    .One => !check_allowzero_match,
                    .Many => !check_allowzero_match,
                    .Slice => false,
                    .C => true,
                },
            }) break :return_block false;

            // if pointer to array
            if (!isConvertibleTo(from.child, to.child)) break :return_block false;
        }

        break :return_block true;
    };
}
