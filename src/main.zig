//Loop forever:
//  Print prompt: "Press b for backup or r for restore: "
//  Read a line from stdin into a buffer
// If line is empty (when user pressed enter), continue
// Take first character of line
// Normalize to lowercase (so B == b, R == r)
// If it's 'b':
//      Print "Starting backup..."
//      Call runBackup()
//      Break out of loop
// Else if it's 'r':
//      Print "Starting restore..."
//      Call runRestore()
//      Break out of loop
// Else:
//      Print "Please only use r for restore or b for backup."
//      Continue loop

const std = @import("std");

pub fn main() !void {
    //Print intro text and explain options (b/r)
    // make buffer
    var stdout_buffer: [1024]u8 = undefined;
    // create writer
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    // get interface
    const stdout = &stdout_writer.interface;

    // print intro
    try stdout.print("distro-migrater-f2fi starting...\n", .{});
    // clean memory
    try stdout.flush();

    // make buffer for reader and create reader
    var stdin_buffer: [128]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const stdin = &stdin_reader.interface;

    while (true) {
        // 1) print prompt
        try stdout.print("Press b for backup or r for restore: ", .{});
        try stdout.flush();
        // 2) read a line from stdin
        // for later: const raw_line = (try stdin.takeDelimiter('\n')) orelse break;
        const maybe = try stdin.takeDelimiter('\n');
        if (maybe == null) {
            try stdout.print("EOF received, exiting...\n", .{});
            try stdout.flush();
            break;
        }
        const raw_line = maybe.?;

        // trim whitespace and newlines
        const line = std.mem.trim(u8, raw_line, " \t\n\r");

        // 3) if empty -> continue
        if (line.len == 0) {
            continue;
        }
        // 4) char = lowercase(first char)
        const first_char = std.ascii.toLower(line[0]);
        // 5) if b -> confirm + runBackup + break
        if (first_char == 'b') {
            try stdout.print("Starting backup...\n", .{});
            try stdout.flush();
            try runBackup(stdout);
            break;
        }
        // 6) if r -> confirm + runRestore + break
        if (first_char == 'r') {
            try stdout.print("Starting restore...\n", .{});
            try stdout.flush();
            try runRestore(stdout);
            break;
        }
        // 7) else -> print "please only use..."
        try stdout.print("Please only use r for restore or b for backup.\n", .{});
        try stdout.flush();
    }
}

fn runBackup(stdout: anytype) !void {
    // TODO: later this will do environment checks + create tar + manifest
    try stdout.print("TODO: backup not implemented yet.\n", .{});
    try stdout.flush();
}

fn runRestore(stdout: anytype) !void {
    // TODO: later this will reaad manifest + install flatpaks + restore configs
    try stdout.print("TODO: restore not implemented yet.\n", .{});
    try stdout.flush();
}
