const std = @import("std");
const Lexer = @import("Lexer.zig");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var it = std.process.args();
    defer it.deinit();

    // Skip program name
    _ = it.next(allocator);

    const file_path = try (it.next(allocator) orelse {
        printHelp();
        return;
    });

    _ = Lexer.init(allocator, file_path) catch {
        std.debug.print(
            "File does not exist or could not be read '{s}'\n",
            .{file_path},
        );
    };
}

fn printHelp() void {
    std.debug.print("Usage: spruce file_name", .{});
}