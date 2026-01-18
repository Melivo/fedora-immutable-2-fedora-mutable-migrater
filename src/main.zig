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

    // ostree = isOstreeSystem()
    // fedora = isFedoraSystem()
    //print:
    //    ostree: true/false
    //    fedora: true/false

    // if ostree && fedora:
    //    print " OK: Immutable Fedora system detected."
    // else if !ostree && fedora:
    //    print " OK: Mutable Fedora system detected."
    // else if ostree && !fedora:
    //    print " Unsupported: Immutable non-Fedora system detected. Exiting."
    // else:
    //    print " Unsupported: Mutable non-Fedora system detected. Exiting."

    // make buffer for reader and create reader
    var stdin_buffer: [128]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const stdin = &stdin_reader.interface;

    //
    // Detect dnf:
    //   If "dnf" executable exists in PATH -> dnf = true
    //   else -> dnf = false
    //
    // Print detection summary:
    //    ostree: true/false
    //    fedora: true/false
    //    dnf: true/false
    //
    // Decide:
    //    if ostree and fedora -> immutable fedora (OK)
    //    else if fedora and dnf -> mutable fedora (OK)
    //    else -> unsupported (exit)

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

// ostree detection function
// isOstreeSystem():
fn isOstreeSystem() !bool {
    // marker_path = "/run/ostree-booted"
    const marker_path = "/run/ostree-booted";
    // try to open marker_path for reading
    // if open succeeds -> close -> return true
    var file = std.fs.openFileAbsolute(marker_path, .{}) catch |err| {
        // if error.FileNotFound -> return false
        if (err == error.FileNotFound) {
            return false;
            // else -> return the error (bubble up)
        } else {
            return err;
        }
    };
    // if open succeeded: close file and return true
    defer file.close();
    return true;
}

// isFedoraSystem():
//    open "/etc/os-release" for reading
fn isFedoraSystem() !bool {
    const os_release_path = "/etc/os-release";
    // try to open os_release_path for reading
    var file = std.fs.openFileAbsolute(os_release_path, .{}) catch |err| {
        // if error.FileNotFound -> return false
        if (err == error.FileNotFound) {
            return false;
            // all other errors -> bubble up
        } else {
            return err;
        }
    };
    // close file after any exit
    defer file.close();

    // if open succeeded:
    // create buffer
    var buffer: [1024]u8 = undefined;
    // create reader
    var reader = file.reader(&buffer);
    // create interface
    const reader_interface = &reader.interface;
    // read line by line
    const line_delimiter: u8 = '\n';

    while (true) {
        // read line by line (take delimiter '\n')
        const maybe_line = try reader_interface.takeDelimiter(line_delimiter);
        if (maybe_line == null) {
            // EOF reached -> return false
            break;
        }
        const line = maybe_line.?;
        // trim whitespace
        const trimmed_line = std.mem.trim(u8, line, " \t\r\n");

        // if line starts with "ID=":
        const id_prefix = "ID=";
        if (std.mem.startsWith(u8, trimmed_line, id_prefix)) {
            // value = line after "ID="
            const value = trimmed_line[id_prefix.len..];
            // trim quotes from value
            const trimmed_value = std.mem.trim(u8, value, "\"'");
            // if value == "fedora" -> return true and close file
            if (std.mem.eql(u8, trimmed_value, "fedora")) {
                return true;
            }
        }
    }
    return false;
}
