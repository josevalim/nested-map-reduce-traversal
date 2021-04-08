const std = @import("std");

const input =
    \\[
    \\  {
    \\    "title": "Getting started",
    \\    "reset_lesson_position": false,
    \\    "lessons": [
    \\      {"name": "Welcome"},
    \\      {"name": "Installation"}
    \\    ]
    \\  },
    \\
    \\  {
    \\    "title": "Basic operator",
    \\    "reset_lesson_position": false,
    \\    "lessons": [
    \\      {"name": "Addition / Subtraction"},
    \\      {"name": "Multiplication / Division"}
    \\    ]
    \\  },
    \\
    \\  {
    \\    "title": "Advanced topics",
    \\    "reset_lesson_position": true,
    \\    "lessons": [
    \\      {"name": "Mutability"},
    \\      {"name": "Immutability"}
    \\    ]
    \\  }
    \\]
;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    var p = std.json.Parser.init(std.heap.page_allocator, false);
    defer p.deinit();
    var tree = try p.parse(input);
    defer tree.deinit();

    var section_counter: u32 = 1;
    var lesson_counter: u32 = 1;
    for (tree.root.Array.items) |*section| {
        if (section.Object.get("reset_lesson_position").?.Bool) {
            lesson_counter = 1;
        }

        try section.Object.put("position", .{ .Integer = section_counter });
        section_counter += 1;

        for (section.Object.get("lessons").?.Array.items) |*lesson| {
            try lesson.Object.put("position", .{ .Integer = lesson_counter });
            lesson_counter += 1;
        }
    }

    try tree.root.jsonStringify(.{ .whitespace = .{} }, stdout);
}
