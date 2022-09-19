const std = @import("std");
const Allocator = std.mem.Allocator;
const utils = @import("utils.zig");

pub const Lexer = @This();

source: []const u8,
ip: usize,
line: usize,
column: usize,

pub fn init(allocator: Allocator, path: []const u8) !Lexer {
    const source = try utils.readFileContentsAlloc(allocator, path);

    return Lexer {
        .source = source,
        .ip = 0,
        .line = 1,
        .column = 1,
    };
}