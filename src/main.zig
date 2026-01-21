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
    // stdout (buffered)
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const out = &stdout_writer.interface;
    defer out.flush() catch {};

    try out.print("distro-migrater-f2fi starting...\n", .{});

    const ostree = try isOstreeSystem();
    const fedora_like = try isFedoraLikeSystem();

    const kind: SystemKind = if (ostree and fedora_like)
        .immutable_fedora
    else if (!ostree and fedora_like)
        .mutable_fedora
    else if (ostree and !fedora_like)
        .ostree_non_fedora
    else
        .mutable_non_fedora;

    try out.print("System detection summary:\n", .{});
    try out.print("  ostree: {}\n", .{ostree});
    try out.print("  fedora_like: {}\n", .{fedora_like});

    const supported: SupportedKind = switch (kind) {
        .immutable_fedora => blk: {
            try out.print("Immutable Fedora-like OSTree system detected. Backup mode initializing...\n", .{});
            break :blk .immutable_fedora;
        },
        .mutable_fedora => blk: {
            try out.print("Mutable Fedora-like system detected. Restore mode initializing...\n", .{});
            break :blk .mutable_fedora;
        },
        else => {
            try out.print("Unsupported system detected. Exiting.\n", .{});
            return;
        },
    };

    // stdin (buffered)
    var stdin_buffer: [128]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const in = &stdin_reader.interface;

    while (true) {
        switch (supported) {
            .immutable_fedora => {
                try out.print("Press b to backup your dotfiles and a list of your layered apps and flatpaks: ", .{});
                try out.flush();
            },
            .mutable_fedora => {
                try out.print("Press r to restore your dotfiles and install your layered apps and flatpaks as RPM: ", .{});
                try out.flush();
            },
        }

        const maybe = try in.takeDelimiter('\n');
        if (maybe == null) {
            try out.print("EOF received, exiting...\n", .{});
            break;
        }

        const raw_line = maybe.?;
        const line = std.mem.trim(u8, raw_line, " \t\n\r");
        if (line.len == 0) continue;

        const first_char = std.ascii.toLower(line[0]);

        const maybe_action = parseAction(first_char, supported);
        if (maybe_action == null) {
            try out.print("Invalid action for your system type. Please follow the instructions above.\n", .{});
            continue;
        }

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

// ---------------------DETECTION FUNCTIONS---------------------

fn isOstreeSystem() !bool {
    const marker_path = "/run/ostree-booted";
    var file = std.fs.openFileAbsolute(marker_path, .{}) catch |err| {
        if (err == error.FileNotFound) {
            return false;
        } else {
            return err;
        }
    };
    defer file.close();
    return true;
}

// helper to parse KEY=value or KEY="value" lines from /etc/os-release
fn parseOsReleaseValue(line: []const u8, key: []const u8) ?[]const u8 {
    if (!std.mem.startsWith(u8, line, key)) return null;
    if (line.len <= key.len or line[key.len] != '=') return null;

    const raw = line[(key.len + 1)..];
    const trimmed = std.mem.trim(u8, raw, " \t\r\n\"'");
    return trimmed;
}

// returns true if ID == "fedora" OR ID_LIKE contains "fedora"
fn isFedoraLikeSystem() !bool {
    const os_release_path = "/etc/os-release";

    var file = std.fs.openFileAbsolute(os_release_path, .{}) catch |err| {
        if (err == error.FileNotFound) {
            return false;
        } else {
            return err;
        }
    };
    defer file.close();

    var buffer: [1024]u8 = undefined;
    var reader = file.reader(&buffer);
    const r = &reader.interface;

    var found_id: ?[]const u8 = null;
    var found_id_like: ?[]const u8 = null;

    while (true) {
        const maybe_line = try r.takeDelimiter('\n');
        if (maybe_line == null) break;

        const line = std.mem.trim(u8, maybe_line.?, " \t\r\n");
        if (line.len == 0) continue;

        if (found_id == null) {
            if (parseOsReleaseValue(line, "ID")) |v| {
                found_id = v;
                std.debug.print("Detected OS ID: {s}\n", .{v});
            }
        }

        if (found_id_like == null) {
            if (parseOsReleaseValue(line, "ID_LIKE")) |v| {
                found_id_like = v;
                std.debug.print("Detected OS ID_LIKE: {s}\n", .{v});
            }
        }

        if (found_id != null and found_id_like != null) break;
    }

    if (found_id) |id| {
        if (std.mem.eql(u8, id, "fedora")) return true;
    }

    if (found_id_like) |like| {
        var it = std.mem.tokenizeScalar(u8, like, ' ');
        while (it.next()) |tok_raw| {
            const tok = std.mem.trim(u8, tok_raw, " \t\r\n\"'");
            if (std.mem.eql(u8, tok, "fedora")) return true;
        }
    }

    return false;
}

// ---------------------ACTION HANDLING + RUNNERS---------------------

const Action = enum {
    backup,
    restore,
};

fn parseAction(char: u8, kind: SupportedKind) ?Action {
    switch (kind) {
        .immutable_fedora => {
            if (char == 'b') return .backup;
            return null;
        },
        .mutable_fedora => {
            if (char == 'r') return .restore;
            return null;
        },
    }
}

fn runCmdAlloc(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
) ![]u8 {
    var child = std.process.Child.init(argv, allocator);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    const stdout_bytes = try child.stdout.?.readToEndAlloc(allocator, 1024 * 1024 * 8); // 8MB cap
    _ = try child.wait();

    return stdout_bytes;
}

// Write a RELATIVE path (relative to the directory where the program is executed: cwd).
fn writeFileRel(path: []const u8, data: []const u8) !void {
    var f = try std.fs.cwd().createFile(path, .{ .truncate = true });
    defer f.close();
    try f.writeAll(data);
}

// ---------------------BACKUP/RESTORE FUNCTIONS---------------------

// MVP: create folder, dump flatpak list, dump rpm-ostree status
fn runBackup(out: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const ts = std.time.timestamp();
    const dir_name = try std.fmt.allocPrint(a, "backups/backup-{d}", .{ts});

    // ensure ./backups exists
    std.fs.cwd().makePath("backups") catch |err| {
        if (err != error.PathAlreadyExists) return err;
    };

    // ensure ./backups/backup-<ts> exists
    std.fs.cwd().makePath(dir_name) catch |err| {
        if (err != error.PathAlreadyExists) return err;
    };

    try out.print("Backup directory created at: {s}\n", .{dir_name});

    // flatpak list
    const flatpak_out = try runCmdAlloc(a, &.{
        "flatpak",
        "list",
        "--app",
        "--columns=application",
    });
    const flatpak_path = try std.fmt.allocPrint(a, "{s}/flatpak-list.txt", .{dir_name});
    try writeFileRel(flatpak_path, flatpak_out);
    try out.print("Wrote {s}\n", .{flatpak_path});

    // rpm-ostree status
    const ostree_out = try runCmdAlloc(a, &.{
        "rpm-ostree",
        "status",
    });
    const ostree_path = try std.fmt.allocPrint(a, "{s}/rpm-ostree-status.txt", .{dir_name});
    try writeFileRel(ostree_path, ostree_out);
    try out.print("Wrote {s}\n", .{ostree_path});

    try out.print("Backup MVP done (lists only).\n", .{});
}

fn runRestore(stdout: anytype) !void {
    try stdout.print("Restore function not yet implemented.\n", .{});
}
