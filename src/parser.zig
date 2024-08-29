const std = @import("std");
const tok = @import("lexer.zig");
const Token = tok.TokenTag;

const Parser = struct {
    const Self = @This();
    token: Token,

    pub fn parse(self: *Self) void {
        self.consume_token();
        self.block();
        self.expect(.tk_period);
    }

    fn block(self: *Self) void {
        if (self.try_consume(.kw_const)) {
            while (true) {
                self.expect(.identifier);
                self.expect(.assign);
                self.expect(.literal_float);
                if (!self.try_consume(.comma)) break;
            }
            self.expect(.semicolon);
        }
        if (self.try_consume(.tk_var)) {
            while (true) {
                self.expect(.identifier);
                if (!self.try_consume(.identifier)) break;
            }
            self.expect(.semicolon);
        }
        while (self.try_consume(.tk_proc)) {
            self.expect(.identifier);
            self.expect(.semicolon);
            self.block();
            self.expect(.semicolon);
        }
        statement();
    }

    fn statement(self: *Self) void {
        if (self.try_consume(.keyword_if)) {
            self.expect(.left_paren);
            self.condition();
            self.expect(.right_paren);
            self.expect(.left_brace);
            self.block();
            self.expect(.right_brace);
        }

        // if (self.try_consume(.identifier)) {
        //     self.expect(.assign);
        //     self.expression();
        // } else if (self.try_consume(.tk_call)) {
        //     self.expect(.identifier);
        // } else if (self.try_consume(.tk_begin)) {
        //     while (true) {
        //         self.statement();
        //         if (!self.try_consume(.semicolon)) break;
        //     }
        //     self.expect(.tk_end);
        // } else if (self.try_consume(.kw_if)) {
        //     self.condition();
        //     self.expect(.tk_then);
        //     self.statement();
        // } else if (self.try_consume(.kw_while)) {
        //     self.condition();
        //     self.expect(.tk_do);
        //     self.statement();
        // } else {
        //     error_("statement: syntax error");
        //     self.consume_token();
        // }
    }

    fn condition(self: *Self) void {
        if (self.try_consume(.tk_odd)) {
            self.expression();
        } else {
            self.expression();
            switch (self.token) {
                .equal, .not_equal, .less, .less_or_equal, .greater, .greater_or_equal => {
                    self.consume_token();
                    expression();
                },
                else => {
                    error_("condition: invalid operator");
                    self.consume_token();
                },
            }
        }
    }

    fn expression(self: *Self) void {
        if (self.token == .plus or self.token == .minus) {
            self.consume_token();
        }
        self.term();
        while (self.token == .plus or self.token == .minus) {
            self.consume_token();
            self.term();
        }
    }

    fn term(self: *Self) void {
        value();
        while (self.token == .asterisk or self.token == .slash) {
            self.consume_token();
            value();
        }
    }

    fn value(self: *Self) void {
        switch (self.token) {
            .identifier, .literal_float => {
                self.consume_token();
            },
            .left_paren => {
                self.consume_token();
                self.expression();
                self.expect(.right_paren);
            },
            else => {
                error_("value: syntax error");
                self.consume_token();
            },
        }
    }

    fn consume_token(self: *Self) void {
        _ = .{self};
    }

    fn error_(msg: []const u8) void {
        _ = .{msg};
    }

    fn try_consume(self: *Self, token: Token) bool {
        if (self.token == token) {
            self.consume_token();
            return true;
        }
        return false;
    }

    fn expect(self: Self, token: Token) bool {
        if (self.try_consume(token)) return true;
        error_("expect: unexpected symbol");
        return false;
    }
};
