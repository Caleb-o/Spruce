const std = @import("std");
const Allocator = std.mem.Allocator;

// Read an entire file from the given path
pub fn readFileContentsAlloc(allocator: Allocator, path: []const u8) ![]u8 {
    const file_path = try std.fs.realpathAlloc(allocator, path);

    const file = try std.fs.openFileAbsolute(
        file_path,
        .{ .read = true, },
    );
    defer file.close();
    const stat = try file.stat();

    return try file.reader().readAllAlloc(
        allocator,
        stat.size,
    );
}