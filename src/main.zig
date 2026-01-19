const std = @import("std");

const SystemKind = enum {
    mutable_fedora,
    immutable_fedora,
    ostree_non_fedora,
    mutable_non_fedora,
};

const SupportedKind = enum {
    mutable_fedora,
    immutable_fedora,
};

pub fn main() !void {
    // create a buffered writer for stdout
    // make buffer
    var stdout_buffer: [1024]u8 = undefined;
    // create writer
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    // get interface
    const out = &stdout_writer.interface;
    // flush at the end
    defer out.flush() catch {};

    // print intro
    try out.print("distro-migrater-f2fi starting...\n", .{});

    // Detect system type:
    //   ostree = isOstreeSystem()
    const ostree = try isOstreeSystem();
    //   fedora = isFedoraSystem()
    const fedora = try isFedoraSystem();

    // assign results from detection to system kind
    const kind: SystemKind = if (ostree and fedora)
        .immutable_fedora
    else if (!ostree and fedora)
        .mutable_fedora
    else if (ostree and !fedora)
        .ostree_non_fedora
    else
        .mutable_non_fedora;

    // print detection summary for debugging:
    try out.print("System detection summary:\n", .{});
    try out.print("  ostree: {}\n", .{ostree});
    try out.print("  fedora: {}\n", .{fedora});
    // dnf: true/false

    // Implement dnf detection later
    //   If "dnf" executable exists in PATH -> dnf = true
    //   else -> dnf = false

    // print messages for each system type
    // and assign value for supported systems for interactive prompt
    const supported: SupportedKind = switch (kind) {
        .immutable_fedora => blk: {
            try out.print("Immutable Fedora system detected. Backup mode initializing...\n", .{});
            break :blk .immutable_fedora;
        },
        .mutable_fedora => blk: {
            try out.print("Mutable Fedora system detected. Restore mode initializing...\n", .{});
            break :blk .mutable_fedora;
        },
        else => {
            try out.print("Unsupported system detected. Exiting.\n", .{});
            return;
        },
    };

    // ---------------------INTERACTIVE PROMPT---------------------
    // make buffer for reader and create reader
    var stdin_buffer: [128]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const in = &stdin_reader.interface;

    // start interactive prompt loop
    while (true) {
        switch (supported) {
            // Give instructions to backup if immutable fedora
            .immutable_fedora => {
                try out.print("Press b to backup your dotfiles and a list of your layered apps and flatpaks: ", .{});
                try out.flush();
            },
            // Give instructions to restore if mutable fedora
            .mutable_fedora => {
                try out.print("Press r to restore your dotfiles and install your layered apps and flatpaks as RPM: ", .{});
                try out.flush();
            },
        }

        // read a line from stdin
        const maybe = try in.takeDelimiter('\n');
        if (maybe == null) {
            try out.print("EOF received, exiting...\n", .{});
            break;
        }

        // save untrimmed line
        const raw_line = maybe.?;

        // trim whitespace and newlines
        const line = std.mem.trim(u8, raw_line, " \t\n\r");

        // if empty -> continue
        if (line.len == 0) {
            continue;
        }
        // char = lowercase(first char)
        const first_char = std.ascii.toLower(line[0]);

        // create variable for allowed action
        const maybe_action = parseAction(first_char, supported);

        // handle invalid actions
        if (maybe_action == null) {
            // print invalid action message
            try out.print("Invalid action for your system type. Please follow the instructions above.\n", .{});
            continue;
        }

        // handle valid actions
        const action = maybe_action.?;

        switch (action) {
            .backup => {
                try out.print("Starting backup...\n", .{});
                try runBackup(out);
                try out.print("Backup completed successfully.\n", .{});
            },
            .restore => {
                try out.print("Starting restore...\n", .{});
                try runRestore(out);
                try out.print("Restore completed successfully.\n", .{});
            },
        }
        break;
    }
}

// ---------------------HELPER FUNCTIONS---------------------

// ostree detection function
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

// fedora detection function
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
    //  create buffer
    var buffer: [1024]u8 = undefined;
    //  create reader
    var reader = file.reader(&buffer);
    //  create interface
    const reader_interface = &reader.interface;
    //  read line by line
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

// ---------------------ACTION HANDLING + RUNNERS---------------------

// classify allowed action per system type
const Action = enum {
    backup,
    restore,
};

// call allowed action functions for given input char and system kind
fn parseAction(char: u8, kind: SupportedKind) ?Action {
    switch (kind) {
        .immutable_fedora => {
            if (char == 'b') {
                return .backup;
            } else {
                return null;
            }
        },
        .mutable_fedora => {
            if (char == 'r') {
                return .restore;
            } else {
                return null;
            }
        },
    }
}

// TODO: later this will do environment checks + create tar + manifest
fn runBackup(stdout: anytype) !void {
    try stdout.print("Backup function not yet implemented.\n", .{});
}

// TODO: later this will reaad manifest + install flatpaks + restore configs
fn runRestore(stdout: anytype) !void {
    try stdout.print("Restore function not yet implemented.\n", .{});
}
