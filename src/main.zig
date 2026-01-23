// distro-migrater-f2fi: Fedora immutable (based) to Fedora mutable migration tool
//
// Backup mode (on immutable):
//   - Creates ./backups/backup-<unix_ts>/
//   - Captures:
//       1) flatpak-list.txt
//       2) layered-rpms.txt               <-- CHANGED: minimal output instead of huge rpm-ostree JSON
//       3) flatpak-configs/<app_id>/config
//       4) dot-config/                    <-- NEW: backup of ~/.config
//
// Restore mode (on mutable):
//   - Picks a backup folder
//   - Installs layered RPMs via dnf (from layered-rpms.txt; fallback to old rpm-ostree-status.json)
//   - Converts Flatpaks to RPM candidates (interactive)
//   - Restores Flatpak configs into ~/.config/<name>/
//   - Restores dot-config into ~/.config/ (merge)
//
// Design principle:
//   - No fuzzy auto-install without user consent.
//   - If ambiguous: ask. If unknown: report and skip.
//
// CAVEAT: needs sudo for dnf and rpm-ostree commands.

// Zig standard library: filesystem, process, json, etc.
const std = @import("std");

// ArrayListManaged is just another name for std.array_list.Managed,
// so I don't have to write the full path everywhere.
// This is a growable array that uses an allocator for memory.
const ArrayListManaged = std.array_list.Managed;

// create enum list for easier system type handling
// An enum is a small set of named values.
const SystemKind = enum {
    mutable_fedora,
    immutable_fedora,
    ostree_non_fedora,
    mutable_non_fedora,
};

// This enum only includes cases we can actually run on.
const SupportedKind = enum {
    mutable_fedora,
    immutable_fedora,
};

// Two possible actions the user can choose.
const Action = enum { backup, restore };

// FlatpakEntry just groups three strings from one line.
const FlatpakEntry = struct {
    app_id: []const u8,
    origin: []const u8,
    branch: []const u8,
};

const SkipReason = enum {
    not_in_repo,
    could_not_resolve,
};

const SkippedApp = struct {
    app_id: []const u8,
    reason: SkipReason,
};

const ResolveResult = struct {
    chosen: ?[]const u8,
    skip_reason: ?SkipReason,
};

const UserIds = struct {
    name: []const u8,
    uid: u32,
    gid: u32,
};

// command result (stdout + stderr + termination info)
// We keep both outputs plus how the command exited.
const CmdResult = struct {
    stdout: []u8,
    stderr: []u8,
    term: std.process.Child.Term,
};

// Simple "tee" writer: prints to stdout and to a log file.
// This lets us keep all terminal output in a log.
const TeeOut = struct {
    stdout: *std.io.Writer,
    log: *std.io.Writer,

    // Print to both destinations.
    pub fn print(self: *TeeOut, comptime fmt: []const u8, args: anytype) !void {
        try self.stdout.print(fmt, args);
        try self.log.print(fmt, args);
    }

    // Flush both outputs (useful for prompts).
    pub fn flush(self: *TeeOut) !void {
        try self.stdout.flush();
        try self.log.flush();
    }
};

// `!void` means it can return an error instead of a value.
pub fn main() !void {
    // Make buffered writer for stdout.
    // We buffer so prints are grouped efficiently.
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    // Create a log file in the current working directory.
    // Name format: log-<unix_timestamp>.log
    var log_name_buf: [64]u8 = undefined;
    const log_name = try std.fmt.bufPrint(&log_name_buf, "log-{d}.log", .{std.time.timestamp()});
    var log_file = try std.fs.cwd().createFile(log_name, .{ .truncate = true });
    defer log_file.close();

    // Buffered log writer for smoother file output.
    var log_buffer: [1024]u8 = undefined;
    var log_writer = log_file.writer(&log_buffer);

    // Use TeeOut so every print goes to both stdout and the log.
    var tee = TeeOut{
        .stdout = &stdout_writer.interface,
        .log = &log_writer.interface,
    };
    const out = &tee;
    // `defer` runs at the end of the scope (like cleanup in C).
    defer out.flush() catch {};

    // print startup message
    try out.print("distro-migrater-f2fi starting...\n", .{});

    // assign system detection results to variables
    // `try` means: if this errors, return the error from main.
    const ostree = try isOstreeSystem();
    const fedora_like = try isFedoraLikeSystem();

    // define system kind based on detection results
    // We combine the two booleans into one enum value.
    const kind: SystemKind = if (ostree and fedora_like)
        .immutable_fedora
    else if (!ostree and fedora_like)
        .mutable_fedora
    else if (ostree and !fedora_like)
        .ostree_non_fedora
    else
        .mutable_non_fedora;

    // print detection summary
    try out.print("System detection summary:\n", .{});
    try out.print("  ostree: {}\n", .{ostree});
    try out.print("  fedora_like: {}\n", .{fedora_like});

    // inform the user about supported/unsupported systems
    // `switch` picks one branch based on the enum value.
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

    // make buffered reader for stdin
    // This lets us read user input line by line.
    var stdin_buffer: [256]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const in = &stdin_reader.interface;

    // Generic warning about desktop environment mismatch.
    try out.print(
        "Warning: restoring configs between different desktop environments (e.g., KDE -> GNOME) can cause issues.\n",
        .{},
    );

    // inform user about available actions and prompt
    // Loop until we get a valid action or EOF.
    while (true) {
        switch (supported) {
            .immutable_fedora => {
                try out.print("Press b to backup your dotfiles and a list of your layered apps and flatpaks: ", .{});
                try out.flush();
            },
            .mutable_fedora => {
                try out.print("Press r to restore (dnf install layered RPMs + convert flatpaks to RPMs): ", .{});
                try out.flush();
            },
        }

        // read user input line
        // `takeDelimiter` returns an optional because it can hit EOF.
        const maybe = try in.takeDelimiter('\n');
        if (maybe == null) {
            try out.print("EOF received, exiting...\n", .{});
            break;
        }

        // process input line
        // `maybe.?` unwraps the optional; we know it is not null here.
        const raw_line = maybe.?;
        const line = std.mem.trim(u8, raw_line, " \t\n\r");
        if (line.len == 0) continue;

        // unify input to lowercase for first character
        const first_char = std.ascii.toLower(line[0]);

        // parse action based on first character and supported system kind
        // This returns null if the input is not allowed.
        const maybe_action = parseAction(first_char, supported);
        if (maybe_action == null) {
            try out.print("Invalid action for your system type. Please follow the instructions above.\n", .{});
            continue;
        }

        // After the null check, Zig still sees `maybe_action` as optional (?Action); `switch` needs a real Action.
        // We unwrap once into a normal Action value.
        const action = maybe_action.?;

        // Execute the selected action; each branch runs its workflow and prints progress.
        switch (action) {
            .backup => {
                try out.print("Starting backup...\n", .{});
                try runBackup(out);
                try out.print("Backup completed successfully.\n", .{});
            },
            .restore => {
                try out.print("Starting restore...\n", .{});
                try ensureRootOrReexec(out);
                try runRestore(out, in);
                try out.print("Restore flow completed.\n", .{});
            },
        }

        break;
    }
}

// ---------------------DETECTION FUNCTIONS---------------------

fn isOstreeSystem() !bool {
    // check for existence of /run/ostree-booted
    // If the file exists, we assume an OSTree system.
    const marker_path = "/run/ostree-booted";
    // existence check (file) WITHOUT leaking the handle
    var file = std.fs.openFileAbsolute(marker_path, .{}) catch |err| {
        if (err == error.FileNotFound) return false;
        return err;
    };
    defer file.close();
    return true;
}

// helper to parse KEY=value or KEY="value" lines from /etc/os-release
fn parseOsReleaseValue(line: []const u8, key: []const u8) ?[]const u8 {
    // check if line starts with key=
    // Example line: ID=fedora
    if (!std.mem.startsWith(u8, line, key)) return null;
    // check for '=' after key
    if (line.len <= key.len or line[key.len] != '=') return null;

    // extract raw value and trim quotes + whitespace
    // This handles ID="fedora" as well as ID=fedora.
    const raw = line[(key.len + 1)..];
    return std.mem.trim(u8, raw, " \t\r\n\"'");
}

// returns true if ID == "fedora" OR ID_LIKE contains "fedora"
fn isFedoraLikeSystem() !bool {
    // open and read /etc/os-release
    // This file is standard on Linux and describes the distro.
    const os_release_path = "/etc/os-release";

    // existence check (file) WITHOUT leaking the handle
    var file = std.fs.openFileAbsolute(os_release_path, .{}) catch |err| {
        if (err == error.FileNotFound) return false;
        return err;
    };
    defer file.close();

    //make buffered reader
    // This lets us read the file line by line.
    var buffer: [1024]u8 = undefined;
    var reader = file.reader(&buffer);
    const r = &reader.interface;

    // parse lines for ID and ID_LIKE
    // These two keys tell us if the OS is Fedora or similar.
    var found_id: ?[]const u8 = null;
    var found_id_like: ?[]const u8 = null;

    // read lines
    // Stop early once we have both fields.
    while (true) {
        const maybe_line = try r.takeDelimiter('\n');
        if (maybe_line == null) break;

        // trim line
        // Removes spaces and newline characters.
        const line = std.mem.trim(u8, maybe_line.?, " \t\r\n");
        if (line.len == 0) continue;

        // parse ID and ID_LIKE
        // We only set them once.
        if (found_id == null) {
            if (parseOsReleaseValue(line, "ID")) |v| found_id = v;
        }
        if (found_id_like == null) {
            if (parseOsReleaseValue(line, "ID_LIKE")) |v| found_id_like = v;
        }

        if (found_id != null and found_id_like != null) break;
    }

    // check for "fedora"
    // If ID is exactly "fedora", we are Fedora-like.
    if (found_id) |id| {
        if (std.mem.eql(u8, id, "fedora")) return true;
    }

    // check ID_LIKE tokens
    // ID_LIKE can list multiple words separated by spaces.
    if (found_id_like) |like| {
        // tokenize by spaces
        var it = std.mem.tokenizeScalar(u8, like, ' ');
        // check each token
        while (it.next()) |tok_raw| {
            // trim quotes + whitespace
            const tok = std.mem.trim(u8, tok_raw, " \t\r\n\"'");
            // check for "fedora"
            if (std.mem.eql(u8, tok, "fedora")) return true;
        }
    }

    return false;
}

// ---------------------ACTION HANDLING + RUNNERS---------------------

// parse action based on first character and supported system kind
// Returns null when the action is not allowed for this system.
fn parseAction(char: u8, kind: SupportedKind) ?Action {
    switch (kind) {
        .immutable_fedora => {
            // Only allow backup on immutable systems.
            if (char == 'b') return .backup;
            return null;
        },
        .mutable_fedora => {
            // Only allow restore on mutable systems.
            if (char == 'r') return .restore;
            return null;
        },
    }
}

// Ensure restore runs as root; if not, re-exec via sudo.
fn ensureRootOrReexec(out: anytype) !void {
    if (std.posix.geteuid() == 0) return;

    try out.print("Restore requires root. Re-executing via sudo...\n", .{});

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const args = try std.process.argsAlloc(a);
    var argv = ArrayListManaged([]const u8).init(a);
    defer argv.deinit();
    try argv.append("sudo");
    try argv.append("--");
    for (args) |arg| try argv.append(arg);

    var child = std.process.Child.init(argv.items, a);
    child.stdin_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    child.spawn() catch |err| {
        if (err == error.FileNotFound) {
            try out.print("sudo not found. Please re-run as root.\n", .{});
        }
        return err;
    };

    const term = try child.wait();
    switch (term) {
        .Exited => |code| std.process.exit(code),
        .Signal => |sig| std.process.exit(128 + @as(u8, @intCast(sig))),
        else => std.process.exit(1),
    }
}

// run command and capture stdout + stderr
fn runCmdAlloc(allocator: std.mem.Allocator, argv: []const []const u8) !CmdResult {
    // argv[0] is the program to run; the rest are its arguments. This sets up that external command.
    // We also choose how it handles input/output.
    // Example argv: {"flatpak", "list", "--app"}.
    var child = std.process.Child.init(argv, allocator);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    // spawn the command
    try child.spawn();

    // stdout/stderr are optional in Zig; since we set them to .Pipe above, we can unwrap and read them.
    // `readToEndAlloc` reads all output into a buffer we must later free.
    const stdout_bytes = try child.stdout.?.readToEndAlloc(allocator, 1024 * 1024 * 8); // 8MB cap
    const stderr_bytes = try child.stderr.?.readToEndAlloc(allocator, 1024 * 1024 * 2); // 2MB cap
    const term = try child.wait();

    // Return what the command printed plus how it exited.
    return .{ .stdout = stdout_bytes, .stderr = stderr_bytes, .term = term };
}

// run command and inherit stdout/stderr (avoids pipe deadlocks for chatty commands)
fn runCmdInherit(allocator: std.mem.Allocator, argv: []const []const u8) !std.process.Child.Term {
    var child = std.process.Child.init(argv, allocator);
    child.stdin_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;
    try child.spawn();
    return try child.wait();
}

// enforce success so you don’t create “valid-looking but incomplete” backups/restores
fn runCmdCheckedAlloc(
    // allocator for temporary output buffers
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    out: anytype,
) ![]u8 {
    // `anytype` means this works with any writer that has `print`.
    const res = try runCmdAlloc(allocator, argv);
    errdefer allocator.free(res.stdout);
    errdefer allocator.free(res.stderr);

    // Check how the command terminated.
    switch (res.term) {
        .Exited => |code| {
            if (code != 0) {
                // Non-zero exit means the command failed.
                try out.print("Command failed (exit code {d}):", .{code});
                for (argv) |arg| try out.print(" {s}", .{arg});
                try out.print("\n", .{});

                if (res.stderr.len != 0) {
                    try out.print("stderr:\n{s}\n", .{res.stderr});
                }
                return error.CommandFailed;
            }
        },
        else => {
            // Crashes or signals also count as failure.
            try out.print("Command did not exit normally:", .{});
            for (argv) |arg| try out.print(" {s}", .{arg});
            try out.print("\n", .{});
            if (res.stderr.len != 0) {
                try out.print("stderr:\n{s}\n", .{res.stderr});
            }
            return error.CommandFailed;
        },
    }

    // We keep stdout but can free stderr now.
    allocator.free(res.stderr);
    return res.stdout;
}

// Run a command with inherited stdout/stderr and enforce success.
fn runCmdCheckedInherit(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    out: anytype,
) !void {
    const term = try runCmdInherit(allocator, argv);
    switch (term) {
        .Exited => |code| {
            if (code != 0) {
                try out.print("Command failed (exit code {d}):", .{code});
                for (argv) |arg| try out.print(" {s}", .{arg});
                try out.print("\n", .{});
                return error.CommandFailed;
            }
        },
        else => {
            try out.print("Command did not exit normally:", .{});
            for (argv) |arg| try out.print(" {s}", .{arg});
            try out.print("\n", .{});
            return error.CommandFailed;
        },
    }
}

// ----------------------FILE HELPERS---------------------

// write data to file in current directory
fn writeFileRel(path: []const u8, data: []const u8) !void {
    // Create or overwrite a file in the current directory.
    var f = try std.fs.cwd().createFile(path, .{ .truncate = true });
    defer f.close();
    try f.writeAll(data);
}

// write multiple lines to file in current directory
fn writeLinesRel(path: []const u8, lines: []const []const u8) !void {
    // NEW: helper for writing small “relevant output” files (one item per line).
    // `lines` is a slice of strings; each becomes one line in the file.
    var f = try std.fs.cwd().createFile(path, .{ .truncate = true }); // CHANGED: new helper
    defer f.close();

    // write each line followed by newline
    for (lines) |line| {
        try f.writeAll(line);
        try f.writeAll("\n");
    }
}

// ---------------------BACKUP HELPERS---------------------

// get $HOME environment variable
fn getHomeDir(a: std.mem.Allocator) ![]const u8 {
    // If running with sudo, prefer the original user's home.
    if (std.process.getEnvVarOwned(a, "SUDO_USER")) |sudo_user| {
        defer a.free(sudo_user);
        if (try lookupHomeFromPasswd(a, sudo_user)) |home| {
            return home;
        }
    } else |_| {}

    // Fallback: use $HOME from the current environment.
    return try std.process.getEnvVarOwned(a, "HOME");
}

// Look up a user's home directory by reading /etc/passwd.
// This avoids restoring configs into /root when running under sudo.
fn lookupHomeFromPasswd(a: std.mem.Allocator, user: []const u8) !?[]const u8 {
    var file = std.fs.openFileAbsolute("/etc/passwd", .{}) catch |err| {
        if (err == error.FileNotFound) return null;
        return err;
    };
    defer file.close();

    var buffer: [1024]u8 = undefined;
    var reader = file.reader(&buffer);
    const r = &reader.interface;

    while (true) {
        const maybe_line = try r.takeDelimiter('\n');
        if (maybe_line == null) break;
        const line = std.mem.trim(u8, maybe_line.?, " \t\r\n");
        if (line.len == 0) continue;

        // /etc/passwd lines are: name:x:uid:gid:gecos:home:shell
        if (!std.mem.startsWith(u8, line, user)) continue;
        if (line.len <= user.len or line[user.len] != ':') continue;

        var parts = std.mem.splitScalar(u8, line, ':');
        _ = parts.next(); // name
        _ = parts.next(); // x
        _ = parts.next(); // uid
        _ = parts.next(); // gid
        _ = parts.next(); // gecos
        const home = parts.next() orelse return null;

        // Return an owned copy of the home path.
        return try a.dupe(u8, home);
    }

    return null;
}

// Look up a user's uid and gid by reading /etc/passwd.
fn lookupUserIdsFromPasswd(a: std.mem.Allocator, user: []const u8) !?UserIds {
    // Read /etc/passwd and find the line for the given user.
    // That line contains uid/gid as plain numbers, which we parse below.
    var file = std.fs.openFileAbsolute("/etc/passwd", .{}) catch |err| {
        if (err == error.FileNotFound) return null;
        return err;
    };
    defer file.close();

    // make buffered reader
    var buffer: [1024]u8 = undefined;
    var reader = file.reader(&buffer);
    const r = &reader.interface;

    // Read the file line-by-line because /etc/passwd is a text file.
    while (true) {
        const maybe_line = try r.takeDelimiter('\n');
        if (maybe_line == null) break;
        const line = std.mem.trim(u8, maybe_line.?, " \t\r\n");
        if (line.len == 0) continue;

        // /etc/passwd lines are: name:x:uid:gid:gecos:home:shell
        if (!std.mem.startsWith(u8, line, user)) continue;
        if (line.len <= user.len or line[user.len] != ':') continue;

        // Split on ':' to pull out the uid and gid fields.
        var parts = std.mem.splitScalar(u8, line, ':');
        _ = parts.next(); // name
        _ = parts.next(); // x
        const uid_str = parts.next() orelse return null;
        const gid_str = parts.next() orelse return null;
        _ = parts.next(); // gecos
        _ = parts.next(); // home
        _ = parts.next(); // shell

        // Convert uid/gid from strings to integers so we can pass them to chown.
        const uid = std.fmt.parseInt(u32, uid_str, 10) catch return null;
        const gid = std.fmt.parseInt(u32, gid_str, 10) catch return null;
        return .{
            .name = try a.dupe(u8, user),
            .uid = uid,
            .gid = gid,
        };
    }

    // No matching user found in /etc/passwd.
    return null;
}

// Parse the flatpak-list output: tab-separated columns
fn parseFlatpakListFromBytes(a: std.mem.Allocator, data: []const u8) ![]FlatpakEntry {
    // Split the text by newline characters.
    var lines = std.mem.splitScalar(u8, data, '\n');

    // Use an ArrayList because we do not know the count ahead of time.
    var entries = ArrayListManaged(FlatpakEntry).init(a);
    defer entries.deinit();

    // Parse each line into three tab-separated columns.
    while (lines.next()) |raw| {
        const line = std.mem.trim(u8, raw, " \t\r\n");
        if (line.len == 0) continue;

        // tokenize by tab
        var cols = std.mem.tokenizeScalar(u8, line, '\t');
        const app_id = cols.next() orelse continue;
        const origin = cols.next() orelse "";
        const branch = cols.next() orelse "";

        // Append one FlatpakEntry to the list.
        try entries.append(.{
            .app_id = app_id,
            .origin = origin,
            .branch = branch,
        });
    }

    // Convert the ArrayList into a slice that owns its memory.
    return try entries.toOwnedSlice();
}

// Backup: copy ~/.var/app/<app_id>/config -> <backupdir>/flatpak-configs/<app_id>/config
fn backupFlatpakConfigs(
    // allocator for temporary strings
    a: std.mem.Allocator,
    out: anytype,
    backup_dir: []const u8,
    flatpak_list_bytes: []const u8,
) !void {
    // Use HOME to build source paths.
    const home = try getHomeDir(a);

    // Parse the list of Flatpaks we will back up.
    const entries = try parseFlatpakListFromBytes(a, flatpak_list_bytes);
    if (entries.len == 0) {
        try out.print("No Flatpaks in list -> skipping Flatpak config backup.\n", .{});
        return;
    }

    // The root folder where all Flatpak configs will be stored.
    const root_rel = try std.fmt.allocPrint(a, "{s}/flatpak-configs", .{backup_dir});
    std.fs.cwd().makePath(root_rel) catch |err| {
        if (err != error.PathAlreadyExists) return err;
    };

    // Track how many configs we successfully copied.
    var copied_count: usize = 0;

    for (entries) |fp| {
        // Build the absolute source path for this app's config.
        const src_abs = try std.fmt.allocPrint(a, "{s}/.var/app/{s}/config", .{ home, fp.app_id });

        // Existence check (dir) WITHOUT leaking the handle
        var src_dir = std.fs.openDirAbsolute(src_abs, .{}) catch |err| {
            if (err == error.FileNotFound) continue; // no config dir -> skip
            return err;
        };
        src_dir.close();

        // Build the destination folder for this app inside the backup.
        const dst_rel = try std.fmt.allocPrint(a, "{s}/{s}", .{ root_rel, fp.app_id });
        std.fs.cwd().makePath(dst_rel) catch |err| {
            if (err != error.PathAlreadyExists) return err;
        };

        // cp -a <src_abs> <dst_rel>/
        // This creates: <dst_rel>/config
        try runCmdCheckedInherit(a, &.{ "cp", "-a", src_abs, dst_rel }, out);

        // Update progress count and log it.
        copied_count += 1;
        try out.print("Backed up Flatpak config: {s} -> {s}/\n", .{ src_abs, dst_rel });
    }

    try out.print("Flatpak config backup done. Copied {d} config dirs.\n", .{copied_count});
}

// NEW: Backup ~/.config into <backup>/dot-config/
fn backupDotConfig(
    // allocator for temporary strings
    a: std.mem.Allocator,
    out: anytype,
    backup_dir: []const u8,
) !void {
    // NEW: backup of ~/.config (general app configs, not only Flatpak).
    // We store it as "<backup>/dot-config/" so restore can merge it into ~/.config/.
    // This is a full copy of the folder contents.
    const home = try getHomeDir(a);
    const src_abs = try std.fmt.allocPrint(a, "{s}/.config", .{home});
    const dst_rel = try std.fmt.allocPrint(a, "{s}/dot-config", .{backup_dir}); // CHANGED/NEW

    // Ensure destination exists.
    try runCmdCheckedInherit(a, &.{ "mkdir", "-p", dst_rel }, out); // NEW

    // Copy the *contents* of ~/.config into dot-config:
    // Using "/." copies contents rather than nesting the directory.
    // This avoids creating a second ".config" folder inside dot-config.
    const src_with_dot = try std.fmt.allocPrint(a, "{s}/.", .{src_abs}); // NEW
    try runCmdCheckedInherit(a, &.{ "cp", "-a", src_with_dot, dst_rel }, out); // NEW

    try out.print("Backed up ~/.config -> {s}\n", .{dst_rel}); // NEW
}

// NEW: extract layered/requested RPMs from rpm-ostree status --json output
fn extractLayeredRpmsFromOstreeJsonBytes(
    // allocator for temporary strings and structures
    a: std.mem.Allocator,
    json_bytes: []const u8,
) ![][]const u8 {
    // NEW: shared helper so backup can create a minimal layered-rpms.txt
    // Parse JSON into a generic tree structure.
    var parsed = try std.json.parseFromSlice(std.json.Value, a, json_bytes, .{}); // NEW
    defer parsed.deinit();

    // The root of the JSON is expected to be an object.
    const root = parsed.value;

    // Find the "deployments" array in the JSON.
    const deployments_v = root.object.get("deployments") orelse return &[_][]const u8{};
    if (deployments_v != .array) return &[_][]const u8{};

    // Choose the booted deployment (or the first one if none are marked booted).
    var chosen: ?std.json.Value = null;
    for (deployments_v.array.items) |dep| {
        if (dep != .object) continue;
        if (dep.object.get("booted")) |b| {
            if (b == .bool and b.bool) {
                chosen = dep;
                break;
            }
        }
    }
    // Fallback to first deployment if none marked booted.
    if (chosen == null and deployments_v.array.items.len != 0) {
        chosen = deployments_v.array.items[0];
    }
    // If no deployment found, return empty list.
    if (chosen == null) return &[_][]const u8{};

    // `chosen` is optional; unwrap now that we checked for null.
    const dep = chosen.?;
    // Ensure the deployment is an object.
    if (dep != .object) return &[_][]const u8{};

    // These keys are where rpm-ostree tends to store “requested/layered” packages.
    const keys = [_][]const u8{
        "requested-packages",
        "packages",
        "requested-base-packages",
        "requested-local-packages",
    };

    // Use a hash map to avoid duplicate package names.
    var pkgs = std.StringHashMap(void).init(a);
    defer pkgs.deinit();

    // Extract package names from the known keys.
    for (keys) |k| {
        if (dep.object.get(k)) |v| {
            if (v == .array) {
                for (v.array.items) |item| {
                    if (item == .string) {
                        const s = std.mem.trim(u8, item.string, " \t\r\n");
                        if (s.len == 0) continue;
                        const owned = try a.dupe(u8, s);
                        _ = try pkgs.put(owned, {});
                    }
                }
            }
        }
    }

    // Move the unique keys into a list so we can sort them.
    var out_list = ArrayListManaged([]const u8).init(a);
    defer out_list.deinit();

    // Iterate over the hash map and collect keys.
    var it = pkgs.iterator();
    while (it.next()) |kv| {
        try out_list.append(kv.key_ptr.*);
    }

    // Sort alphabetically for stable output.
    std.mem.sort([]const u8, out_list.items, {}, struct {
        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
            return std.mem.order(u8, lhs, rhs) == .lt;
        }
    }.lessThan);

    // Return a heap-allocated list of package names.
    return try out_list.toOwnedSlice();
}

// main backup driver
fn runBackup(out: anytype) !void {
    // Arena allocator: free everything at once at the end.
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    // Use current timestamp so each backup folder is unique.
    const ts = std.time.timestamp();
    const dir_name = try std.fmt.allocPrint(a, "backups/backup-{d}", .{ts});

    // Create backup directories if they do not exist.
    std.fs.cwd().makePath("backups") catch |err| {
        if (err != error.PathAlreadyExists) return err;
    };
    std.fs.cwd().makePath(dir_name) catch |err| {
        if (err != error.PathAlreadyExists) return err;
    };

    try out.print("Backup directory created at: {s}\n", .{dir_name});

    // Run `flatpak list` and capture its output.
    const flatpak_out = try runCmdCheckedAlloc(a, &.{
        "flatpak",
        "list",
        "--app",
        "--columns=application,origin,branch",
    }, out);

    // Save the Flatpak list to a file in the backup folder.
    const flatpak_path = try std.fmt.allocPrint(a, "{s}/flatpak-list.txt", .{dir_name});
    try writeFileRel(flatpak_path, flatpak_out);
    try out.print("Wrote {s}\n", .{flatpak_path});

    // Parse the list so we can show the user what was saved.
    const flatpak_entries = try parseFlatpakListFromBytes(a, flatpak_out);
    try out.print("Flatpaks saved ({d}):\n", .{flatpak_entries.len});
    for (flatpak_entries) |fp| {
        // Print just the app id to keep the list readable.
        try out.print("  - {s}\n", .{fp.app_id});
    }

    // Backup Flatpak configs
    try backupFlatpakConfigs(a, out, dir_name, flatpak_out);

    // NEW: Backup ~/.config (general non-Flatpak configs)
    try backupDotConfig(a, out, dir_name); // NEW

    // CHANGED: We still call rpm-ostree status --json, but we do NOT store the huge JSON anymore.
    // Instead we extract only the layered/requested packages and write layered-rpms.txt.
    // Run rpm-ostree status --json to get layered package info.
    const ostree_json = try runCmdCheckedAlloc(a, &.{
        "rpm-ostree",
        "status",
        "--json",
    }, out); // CHANGED: capture JSON only as input for extraction

    // Extract package names and write them to layered-rpms.txt.
    const layered = try extractLayeredRpmsFromOstreeJsonBytes(a, ostree_json); // NEW
    const layered_path = try std.fmt.allocPrint(a, "{s}/layered-rpms.txt", .{dir_name}); // NEW
    try writeLinesRel(layered_path, layered); // NEW: minimal “relevant output”
    try out.print("Wrote {s} ({d} packages)\n", .{ layered_path, layered.len }); // NEW

    try out.print("Backup done.\n", .{});
}

// ---------------------RESTORE---------------------

// main restore driver
fn runRestore(out: anytype, in: anytype) !void {
    // Arena allocator for temporary strings and arrays.
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const restore_user: ?UserIds = blk: {
        if (std.posix.geteuid() != 0) break :blk null;
        const sudo_user = std.process.getEnvVarOwned(a, "SUDO_USER") catch |err| switch (err) {
            error.EnvironmentVariableNotFound => break :blk null,
            else => return err,
        };
        defer a.free(sudo_user);
        if (try lookupUserIdsFromPasswd(a, sudo_user)) |ids| {
            break :blk ids;
        }
        try out.print(
            "Warning: SUDO_USER {s} not found in /etc/passwd; leaving ownership unchanged.\n",
            .{sudo_user},
        );
        break :blk null;
    };

    // Ask the user which backup folder to use.
    const backup_dir = try pickBackupDir(a, out, in);

    // CHANGED: prefer the minimal layered-rpms.txt; fallback to old rpm-ostree-status.json.
    // Load the list of layered RPMs from the backup.
    const layered = try loadLayeredRpms(a, out, backup_dir); // CHANGED
    if (layered.len != 0) {
        try out.print("\nLayered RPMs found in backup ({d}):\n", .{layered.len});
        for (layered) |p| try out.print("  - {s}\n", .{p});
        try out.print("\nInstalling layered RPMs via dnf...\n", .{});
        try dnfInstallPkgs(a, out, layered);
        try out.print("Layered RPM install done.\n", .{});
    } else {
        try out.print("\nNo layered RPMs found in backup.\n", .{});
    }

    // Load the Flatpak list we backed up.
    const flatpaks = try loadFlatpakList(a, out, backup_dir);
    if (flatpaks.len == 0) {
        try out.print("\nNo Flatpaks found in backup.\n", .{});
        // Even if no Flatpaks, we still restore ~/.config because it’s useful.
        try out.print("\nRestoring ~/.config (dot-config) into ~/.config/...\n", .{}); // NEW
        try restoreDotConfig(a, out, backup_dir, restore_user); // NEW
        return;
    }

    try out.print("\nFlatpaks found in backup ({d}). Converting to RPM candidates...\n", .{flatpaks.len});

    // Collect RPM package names chosen by the user.
    var rpm_to_install = ArrayListManaged([]const u8).init(a);
    defer rpm_to_install.deinit();
    // Cache for dnf existence checks to avoid repeated queries.
    var dnf_cache = std.StringHashMap(bool).init(a);
    defer dnf_cache.deinit();
    // Track Flatpaks that did not map to an RPM.
    var skipped_flatpaks = ArrayListManaged(SkippedApp).init(a);
    defer skipped_flatpaks.deinit();
    // Track install status for the final summary.
    var install_status: []const u8 = "not attempted";
    // If an error happens later, still print a summary before exiting.
    errdefer printRestoreSummary(out, rpm_to_install.items, skipped_flatpaks.items, install_status) catch {};

    // For each Flatpak, try to resolve it to an RPM package interactively.
    for (flatpaks) |fp| {
        const result = try resolveFlatpakToRpmInteractive(a, out, in, &dnf_cache, fp);
        if (result.chosen) |pkg_name| {
            try rpm_to_install.append(pkg_name);
        } else {
            // Keep track of skipped apps for the summary.
            try skipped_flatpaks.append(.{
                .app_id = fp.app_id,
                .reason = result.skip_reason.?,
            });
            try out.print(
                "Skipping (manual install needed): {s} (origin={s}, branch={s})\n",
                .{ fp.app_id, fp.origin, fp.branch },
            );
        }
    }

    // Install the selected RPM packages via dnf.
    if (rpm_to_install.items.len != 0) {
        var uniq = std.StringHashMap(void).init(a);
        defer uniq.deinit();
        var rpm_uniq = ArrayListManaged([]const u8).init(a);
        defer rpm_uniq.deinit();
        for (rpm_to_install.items) |p| {
            if (!uniq.contains(p)) {
                _ = try uniq.put(p, {});
                try rpm_uniq.append(p);
            }
        }
        try out.print("\nInstalling selected RPMs ({d}) via dnf...\n", .{rpm_uniq.items.len});
        dnfInstallPkgs(a, out, rpm_uniq.items) catch |err| {
            install_status = "failed";
            return err;
        };
        install_status = "success";
        try out.print("RPM install done.\n", .{});
    } else {
        install_status = "not attempted (no RPMs selected)";
        try out.print("\nNo RPMs selected for installation from Flatpak conversion.\n", .{});
    }

    // Restore Flatpak configs
    try out.print("\nRestoring Flatpak configs into ~/.config/...\n", .{});
    try restoreFlatpakConfigs(a, out, backup_dir, restore_user);

    // NEW: Restore general ~/.config backup (merge)
    try out.print("\nRestoring ~/.config (dot-config) into ~/.config/...\n", .{}); // NEW
    try restoreDotConfig(a, out, backup_dir, restore_user); // NEW

    // Print a clear summary at the end so the user can audit the result.
    try printRestoreSummary(out, rpm_to_install.items, skipped_flatpaks.items, install_status);
}

// list ./backups/backup-* and let user pick
fn pickBackupDir(a: std.mem.Allocator, out: anytype, in: anytype) ![]const u8 {
    // Open the "backups" directory so we can list its subfolders.
    var backups_dir = std.fs.cwd().openDir("backups", .{ .iterate = true }) catch |err| {
        if (err == error.FileNotFound) {
            try out.print("No ./backups directory found. Nothing to restore.\n", .{});
            return error.NoBackups;
        }
        return err;
    };
    defer backups_dir.close();

    // Iterator for directory entries.
    var it = backups_dir.iterate();
    var names = ArrayListManaged([]const u8).init(a);
    defer names.deinit();

    // Collect names of backup-* directories.
    while (try it.next()) |entry| {
        if (entry.kind != .directory) continue;
        if (!std.mem.startsWith(u8, entry.name, "backup-")) continue;

        // Copy the name into allocator-owned memory.
        const copy = try a.dupe(u8, entry.name);
        try names.append(copy);
    }

    // Check if we found any backups.
    if (names.items.len == 0) {
        try out.print("No backup-* directories found under ./backups.\n", .{});
        return error.NoBackups;
    }

    // Sort so the newest-looking name appears first.
    std.mem.sort([]const u8, names.items, {}, struct {
        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
            return std.mem.order(u8, lhs, rhs) == .gt; // newest first
        }
    }.lessThan);

    // Display choices
    try out.print("\nAvailable backups:\n", .{});
    for (names.items, 0..) |n, i| {
        try out.print("  {d}) {s}\n", .{ i + 1, n });
    }

    // Prompt for choice
    while (true) {
        // Ask the user to choose a number from the list.
        try out.print("Choose a backup number: ", .{});
        try out.flush();

        // read user input line
        const maybe_line = try in.takeDelimiter('\n');
        if (maybe_line == null) return error.EOF;

        // Trim whitespace around the input.
        const line = std.mem.trim(u8, maybe_line.?, " \t\r\n");
        if (line.len == 0) continue;

        // Parse the input as an integer.
        const idx = std.fmt.parseInt(usize, line, 10) catch {
            try out.print("Please enter a number.\n", .{});
            continue;
        };

        // Check if the index is in range.
        if (idx == 0 or idx > names.items.len) {
            try out.print("Out of range.\n", .{});
            continue;
        }

        // Build the final path to the chosen backup.
        const chosen_name = names.items[idx - 1];
        return try std.fmt.allocPrint(a, "backups/{s}", .{chosen_name});
    }
}

// ---------------------LAYERED RPM LOADING---------------------

// Load layered RPMs from backup (tries layered-rpms.txt first, then falls back to rpm-ostree-status.json)
fn loadLayeredRpms(a: std.mem.Allocator, out: anytype, backup_dir: []const u8) ![][]const u8 {
    // CHANGED: new main loader: prefer layered-rpms.txt for minimal relevant data.
    // This returns either a list of packages or falls back to old JSON.
    if (try loadLayeredRpmsFromTxt(a, out, backup_dir)) |pkgs| {
        return pkgs;
    }

    // Fallback for older backups that still have rpm-ostree-status.json.
    return try loadLayeredRpmsFromStatusJson(a, out, backup_dir); // CHANGED
}

//  load layered-rpms.txt (one package name per line)
fn loadLayeredRpmsFromTxt(a: std.mem.Allocator, out: anytype, backup_dir: []const u8) !?[][]const u8 {
    // NEW: reads "<backup>/layered-rpms.txt" (one package per line)
    // Returning null here means "file not found, use fallback".
    const path = try std.fmt.allocPrint(a, "{s}/layered-rpms.txt", .{backup_dir});
    var f = std.fs.cwd().openFile(path, .{}) catch |err| {
        if (err == error.FileNotFound) return null; // NEW: signal “use fallback”
        return err;
    };
    defer f.close();

    // Read and parse lines
    // We cap size at 1MB to avoid huge allocations.
    const data = try f.readToEndAlloc(a, 1024 * 1024); // 1MB cap; should be tiny
    var lines = std.mem.splitScalar(u8, data, '\n');

    // Collect package names in a list we can return.
    var list = ArrayListManaged([]const u8).init(a);
    defer list.deinit();

    // Parse lines
    while (lines.next()) |raw| {
        const s = std.mem.trim(u8, raw, " \t\r\n");
        if (s.len == 0) continue;
        try list.append(s);
    }

    // If the file exists but is empty, we still return an empty list (valid case).
    try out.print("Loaded layered RPM list from layered-rpms.txt\n", .{}); // NEW
    return try list.toOwnedSlice();
}

// ---------------------FLATPAK LIST LOADING---------------------

// Load and parse flatpak-list.txt (tab-separated columns)
fn loadFlatpakList(a: std.mem.Allocator, out: anytype, backup_dir: []const u8) ![]FlatpakEntry {
    // Load the flatpak-list.txt file from the backup directory.
    const path = try std.fmt.allocPrint(a, "{s}/flatpak-list.txt", .{backup_dir});
    var f = std.fs.cwd().openFile(path, .{}) catch |err| {
        if (err == error.FileNotFound) {
            try out.print("No flatpak-list.txt in {s}\n", .{backup_dir});
            return &[_]FlatpakEntry{};
        }
        return err;
    };
    defer f.close();

    // Read the file into memory and parse it.
    const data = try f.readToEndAlloc(a, 1024 * 1024 * 2); // 2MB cap
    return try parseFlatpakListFromBytes(a, data);
}

// ---------------------OLD JSON FALLBACK (KEEP FOR COMPAT)---------------------

// Extract only "layered/extra packages" from rpm-ostree status JSON.
// Ignores removals/overrides by design.
fn loadLayeredRpmsFromStatusJson(
    // allocator for temporary strings and structures
    a: std.mem.Allocator,
    out: anytype,
    backup_dir: []const u8,
) ![][]const u8 {
    // CHANGED: renamed/kept as fallback for older backups.
    // This supports backups that still saved rpm-ostree-status.json.
    const path = try std.fmt.allocPrint(a, "{s}/rpm-ostree-status.json", .{backup_dir});
    var f = std.fs.cwd().openFile(path, .{}) catch |err| {
        if (err == error.FileNotFound) {
            try out.print("No rpm-ostree-status.json in {s}\n", .{backup_dir});
            return &[_][]const u8{};
        }
        return err;
    };
    defer f.close();

    // Read and parse JSON
    // The file can be large, so we cap at 16MB.
    const data = try f.readToEndAlloc(a, 1024 * 1024 * 16); // 16MB cap
    var parsed = try std.json.parseFromSlice(std.json.Value, a, data, .{});
    defer parsed.deinit();

    // The root of the JSON is expected to be an object.
    const root = parsed.value;

    // Find the "deployments" array in the JSON.
    const deployments_v = root.object.get("deployments") orelse return &[_][]const u8{};
    if (deployments_v != .array) return &[_][]const u8{};

    // Choose the booted deployment (or the first one if none are marked booted).
    var chosen: ?std.json.Value = null;
    for (deployments_v.array.items) |dep| {
        if (dep != .object) continue;
        if (dep.object.get("booted")) |b| {
            if (b == .bool and b.bool) {
                chosen = dep;
                break;
            }
        }
    }
    // Fallback to first deployment if none are booted.
    if (chosen == null and deployments_v.array.items.len != 0) {
        chosen = deployments_v.array.items[0];
    }
    // If no deployment found, return empty list.
    if (chosen == null) return &[_][]const u8{};

    // `chosen` is optional; unwrap now that we checked for null.
    const dep = chosen.?;
    if (dep != .object) return &[_][]const u8{};

    // These keys are where rpm-ostree tends to store “requested/layered” packages.
    const keys = [_][]const u8{
        "requested-packages",
        "packages",
        "requested-base-packages",
        "requested-local-packages",
    };

    // Hash map used to deduplicate package names.
    var pkgs = std.StringHashMap(void).init(a);
    defer pkgs.deinit();

    // Extract package names from the known keys.
    for (keys) |k| {
        // For each key, check if it exists and is an array.
        if (dep.object.get(k)) |v| {
            if (v == .array) {
                for (v.array.items) |item| {
                    if (item == .string) {
                        const s = std.mem.trim(u8, item.string, " \t\r\n");
                        if (s.len == 0) continue;
                        const owned = try a.dupe(u8, s);
                        _ = try pkgs.put(owned, {});
                    }
                }
            }
        }
    }

    // Convert the map keys into a list.
    var out_list = ArrayListManaged([]const u8).init(a);
    defer out_list.deinit();

    // Iterate over the hash map and collect keys.
    var it = pkgs.iterator();
    while (it.next()) |kv| {
        try out_list.append(kv.key_ptr.*);
    }

    // Sort so output order is stable.
    std.mem.sort([]const u8, out_list.items, {}, struct {
        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
            return std.mem.order(u8, lhs, rhs) == .lt;
        }
    }.lessThan);

    if (out_list.items.len == 0) {
        try out.print("No requested packages detected in status JSON.\n", .{});
    }
    try out.print("Loaded layered RPM list from rpm-ostree-status.json (fallback)\n", .{}); // NEW
    return try out_list.toOwnedSlice();
}

// ---------------------DNF INSTALLATION---------------------

// Install multiple packages via dnf install -y
fn dnfInstallPkgs(a: std.mem.Allocator, out: anytype, pkgs: []const []const u8) !void {
    // If there is nothing to install, return early.
    if (pkgs.len == 0) return;

    // Build the argv list for the `dnf install` command.
    var argv = ArrayListManaged([]const u8).init(a);
    defer argv.deinit();

    // dnf install -y <pkgs...>
    try argv.append("dnf");
    try argv.append("install");
    try argv.append("-y");
    for (pkgs) |p| try argv.append(p);

    // Run dnf with inherited stdout/stderr for live progress output.
    const term = try runCmdInherit(a, argv.items);

    // Treat non-zero exit codes as failure.
    switch (term) {
        .Exited => |code| {
            if (code != 0) {
                try out.print("Command failed (exit code {d}):", .{code});
                for (argv.items) |arg| try out.print(" {s}", .{arg});
                try out.print("\n", .{});
                return error.CommandFailed;
            }
        },
        else => {
            try out.print("Command did not exit normally:", .{});
            for (argv.items) |arg| try out.print(" {s}", .{arg});
            try out.print("\n", .{});
            return error.CommandFailed;
        },
    }
}

// Print an end-of-restore summary so the user can audit results.
fn printRestoreSummary(
    out: anytype,
    rpm_to_install: []const []const u8,
    skipped_flatpaks: []const SkippedApp,
    install_status: []const u8,
) !void {
    try out.print("\nRestore summary:\n", .{});
    try out.print("  RPMs selected: {d}\n", .{rpm_to_install.len});
    try out.print("  RPM install status: {s}\n", .{install_status});
    if (rpm_to_install.len != 0) {
        try out.print("  RPM list:\n", .{});
        for (rpm_to_install) |p| try out.print("    - {s}\n", .{p});
    }
    try out.print("  Flatpaks skipped (manual install): {d}\n", .{skipped_flatpaks.len});
    if (skipped_flatpaks.len != 0) {
        try out.print("  Apps to install manually:\n", .{});
        for (skipped_flatpaks) |fp| {
            const reason_str = switch (fp.reason) {
                .not_in_repo => "not in repo",
                .could_not_resolve => "could not be resolved",
            };
            try out.print("    - {s} (reason: {s})\n", .{ fp.app_id, reason_str });
        }
    }
    try out.print("  Config restore steps completed (see log for details).\n", .{});
}

// ----------------------DOT-CONFIG RESTORE---------------------

// Restore: copy <backupdir>/dot-config/. -> ~/.config/
fn restoreDotConfig(
    a: std.mem.Allocator,
    out: anytype,
    backup_dir: []const u8,
    restore_user: ?UserIds,
) !void {
    // NEW: restore "<backup>/dot-config/" into "~/.config/" as a merge.
    // This is intentionally “copy-on-top”, because ~/.config already exists on the target system.
    // We copy files from the backup into the existing config folder.
    const home = try getHomeDir(a);

    // Source is the backed up dot-config; destination is ~/.config.
    const src_abs = try std.fmt.allocPrint(a, "{s}/dot-config", .{backup_dir}); // NEW
    const dst_abs = try std.fmt.allocPrint(a, "{s}/.config", .{home}); // NEW

    // If dot-config doesn't exist in the backup, skip silently.
    // This makes restore safe for older backups.
    var src_dir = std.fs.cwd().openDir(src_abs, .{}) catch |err| {
        if (err == error.FileNotFound) {
            try out.print("No dot-config folder in backup -> skipping ~/.config restore.\n", .{}); // NEW
            return;
        }
        return err;
    };
    src_dir.close();

    // Ensure destination exists (normally it does, but keep it robust).
    // This avoids errors if ~/.config is missing.
    try runCmdCheckedInherit(a, &.{ "mkdir", "-p", dst_abs }, out); // NEW

    // Copy the *contents* of dot-config into ~/.config.
    const src_with_dot = try std.fmt.allocPrint(a, "{s}/.", .{src_abs}); // NEW
    try runCmdCheckedInherit(a, &.{ "cp", "-a", src_with_dot, dst_abs }, out); // NEW

    try out.print("Restored dot-config -> {s}\n", .{dst_abs}); // NEW
    if (restore_user) |ids| {
        const owner = try std.fmt.allocPrint(a, "{d}:{d}", .{ ids.uid, ids.gid });
        try runCmdCheckedInherit(a, &.{ "chown", "-R", owner, dst_abs }, out);
        try out.print("Adjusted ownership -> {s} ({s})\n", .{ dst_abs, ids.name });
    }
}

// ----------------------FLATPAK CONFIG RESTORE---------------------

// Convert a Flatpak app id like "org.example.App" into a simple folder name.
fn flatpakAppIdToConfigName(a: std.mem.Allocator, app_id: []const u8) ![]const u8 {
    // Split by '.' and collect non-empty segments.
    var segs = ArrayListManaged([]const u8).init(a);
    defer segs.deinit();

    // Iterate segments
    var it = std.mem.splitScalar(u8, app_id, '.');
    while (it.next()) |seg| {
        if (seg.len == 0) continue;
        try segs.append(seg);
    }
    // If no segments, return the original app_id.
    if (segs.items.len == 0) return try a.dupe(u8, app_id);

    // Use the last segment unless it is "desktop".
    const last = segs.items[segs.items.len - 1];
    const chosen = if (std.mem.eql(u8, last, "desktop") and segs.items.len >= 2)
        segs.items[segs.items.len - 2]
    else
        last;

    // Normalize to lowercase and replace underscores.
    return try normalizeToken(a, chosen);
}

// Restore: copy <backupdir>/flatpak-configs/<app_id>/config/. -> ~/.config/<name>/
fn restoreFlatpakConfigs(
    a: std.mem.Allocator,
    out: anytype,
    backup_dir: []const u8,
    restore_user: ?UserIds,
) !void {
    // HOME used to build destination paths.
    const home = try getHomeDir(a);

    // The root folder where all Flatpak configs are stored in the backup.
    const src_root_rel = try std.fmt.allocPrint(a, "{s}/flatpak-configs", .{backup_dir});

    // Open the flatpak-configs folder to iterate subdirectories.
    var dir = std.fs.cwd().openDir(src_root_rel, .{ .iterate = true }) catch |err| {
        if (err == error.FileNotFound) {
            try out.print("No flatpak-configs folder in backup -> skipping Flatpak config restore.\n", .{});
            return;
        }
        return err;
    };
    defer dir.close();

    // Iterate subdirectories
    var it = dir.iterate();
    // Count how many configs we restore.
    var restored_count: usize = 0;

    // Each subdirectory corresponds to a Flatpak app id.
    while (try it.next()) |entry| {
        if (entry.kind != .directory) continue;

        // Each subdirectory name is a Flatpak app id.
        const app_id = entry.name;
        const name = try flatpakAppIdToConfigName(a, app_id);

        // Source is the backed up config path; destination is ~/.config/<name>.
        const src_abs = try std.fmt.allocPrint(a, "{s}/{s}/config", .{ src_root_rel, app_id });
        const dst_abs = try std.fmt.allocPrint(a, "{s}/.config/{s}", .{ home, name });

        // Ensure destination directory exists.
        try runCmdCheckedInherit(a, &.{ "mkdir", "-p", dst_abs }, out);

        // Copy the contents of the config directory.
        const src_with_dot = try std.fmt.allocPrint(a, "{s}/.", .{src_abs});
        try runCmdCheckedInherit(a, &.{ "cp", "-a", src_with_dot, dst_abs }, out);

        if (restore_user) |ids| {
            const owner = try std.fmt.allocPrint(a, "{d}:{d}", .{ ids.uid, ids.gid });
            try runCmdCheckedInherit(a, &.{ "chown", "-R", owner, dst_abs }, out);
        }

        // Update progress count and log it.
        restored_count += 1;
        try out.print("Restored Flatpak config: {s} -> {s}\n", .{ src_abs, dst_abs });
    }

    try out.print("Flatpak config restore done. Restored {d} config dirs.\n", .{restored_count});
}

// ---------------------FLATPAK -> RPM RESOLUTION---------------------

// Interactive resolution of Flatpak app ID to RPM package name
fn resolveFlatpakToRpmInteractive(
    // allocator for temporary strings and arrays
    a: std.mem.Allocator,
    out: anytype,
    in: anytype,
    dnf_cache: *std.StringHashMap(bool),
    fp: FlatpakEntry,
) !ResolveResult {
    // Show the Flatpak we are trying to convert.
    try out.print("\nFlatpak: {s} (origin={s}, branch={s})\n", .{ fp.app_id, fp.origin, fp.branch });

    // Derive candidates
    // We create several possible RPM names from the app id.
    const candidates = try deriveRpmCandidates(a, fp.app_id);

    // Check which candidates exist in dnf
    // This avoids suggesting packages that do not exist.
    var existing = ArrayListManaged([]const u8).init(a);
    defer existing.deinit();

    // Check each candidate
    for (candidates) |cand| {
        if (dnf_cache.get(cand)) |exists| {
            if (exists) try existing.append(cand);
            continue;
        }
        const exists = try dnfPackageExists(a, out, cand);
        try dnf_cache.put(cand, exists);
        if (exists) {
            try existing.append(cand);
        }
    }

    // Filter unique existing candidates
    // Some candidates can be duplicates after normalization.
    var uniq = std.StringHashMap(void).init(a);
    defer uniq.deinit();

    // Collect unique existing candidates
    var unique_existing = ArrayListManaged([]const u8).init(a);
    defer unique_existing.deinit();

    // Iterate existing candidates and filter uniques
    for (existing.items) |p| {
        if (!uniq.contains(p)) {
            _ = try uniq.put(p, {});
            try unique_existing.append(p);
        }
    }

    // Resolve based on number of unique existing candidates
    // 0 means no match; 1 means unique match; more means ask the user.
    if (unique_existing.items.len == 0) {
        try out.print("No unambiguous RPM candidate found.\n", .{});
        try out.print("Action: please install manually.\n", .{});
        return .{ .chosen = null, .skip_reason = .not_in_repo };
    }

    // Unique match
    if (unique_existing.items.len == 1) {
        const chosen = unique_existing.items[0];
        try out.print("Resolved uniquely -> RPM: {s}\n", .{chosen});
        return .{ .chosen = chosen, .skip_reason = null };
    }

    // Multiple matches -> ask user
    try out.print("Multiple RPM candidates found. Choose one:\n", .{});
    for (unique_existing.items, 0..) |p, i| {
        try out.print("  {d}) {s}\n", .{ i + 1, p });
    }
    try out.print("  0) skip (manual install)\n", .{});

    // Prompt loop
    // Keep asking until we get a valid number.
    while (true) {
        try out.print("Your choice: ", .{});
        try out.flush();

        const maybe_line = try in.takeDelimiter('\n');
        if (maybe_line == null) return error.EOF;

        // Trim spaces/newlines from user input.
        const line = std.mem.trim(u8, maybe_line.?, " \t\r\n");
        if (line.len == 0) continue;

        // Convert the string to a number.
        const n = std.fmt.parseInt(usize, line, 10) catch {
            try out.print("Please enter a number.\n", .{});
            continue;
        };

        // 0 means "skip this app".
        if (n == 0) return .{ .chosen = null, .skip_reason = .could_not_resolve };
        if (n > unique_existing.items.len) {
            try out.print("Out of range.\n", .{});
            continue;
        }

        // Valid choice
        const chosen = unique_existing.items[n - 1];
        try out.print("Chosen -> RPM: {s}\n", .{chosen});
        return .{ .chosen = chosen, .skip_reason = null };
    }
}

// Derive possible RPM package names from Flatpak app ID
fn deriveRpmCandidates(a: std.mem.Allocator, app_id: []const u8) ![]const []const u8 {
    // Find the last '.' so we can use the last segment.
    const last_dot = std.mem.lastIndexOfScalar(u8, app_id, '.') orelse 0;
    const last = if (last_dot == 0) app_id else app_id[(last_dot + 1)..];

    // Normalize tokens into RPM-friendly names.
    const norm_last = try normalizeToken(a, last);
    const norm_full = try normalizeToken(a, app_id);

    // Use a dynamic list to collect candidates.
    var list = ArrayListManaged([]const u8).init(a);
    defer list.deinit();

    // Basic candidates
    try list.append(norm_last);

    // Heuristics: drop common suffixes.
    if (std.mem.endsWith(u8, norm_last, "-client")) {
        try list.append(norm_last[0 .. norm_last.len - "-client".len]);
    }
    if (std.mem.endsWith(u8, norm_last, "-desktop")) {
        try list.append(norm_last[0 .. norm_last.len - "-desktop".len]);
    }

    // Full app id normalized
    try list.append(norm_full);

    // Vendor-product combo
    // Example: org.mozilla.Firefox -> mozilla-firefox
    var segs = ArrayListManaged([]const u8).init(a);
    defer segs.deinit();

    // Split app_id by '.'
    var it = std.mem.splitScalar(u8, app_id, '.');
    while (it.next()) |seg| {
        if (seg.len == 0) continue;
        try segs.append(seg);
    }

    // If at least two segments, form vendor-product
    if (segs.items.len >= 2) {
        const vendor = try normalizeToken(a, segs.items[segs.items.len - 2]);
        const product = try normalizeToken(a, segs.items[segs.items.len - 1]);
        const combo = try std.fmt.allocPrint(a, "{s}-{s}", .{ vendor, product });
        try list.append(combo);
    }

    // Unique filter
    // Use a hash map to remove duplicates.
    var uniq = std.StringHashMap(void).init(a);
    defer uniq.deinit();

    // Collect unique candidates
    var out_list = ArrayListManaged([]const u8).init(a);
    defer out_list.deinit();

    for (list.items) |s| {
        if (s.len == 0) continue;
        if (!uniq.contains(s)) {
            _ = try uniq.put(s, {});
            try out_list.append(s);
        }
    }

    // Return the list of unique candidates.
    return try out_list.toOwnedSlice();
}

// Normalize token: lowercase, replace '_' with '-', remove spaces
fn normalizeToken(a: std.mem.Allocator, s: []const u8) ![]const u8 {
    // Allocate a buffer the same size as the input.
    var buf = try a.alloc(u8, s.len);
    var j: usize = 0;
    for (s) |c| {
        const lc = std.ascii.toLower(c);
        const outc: u8 = switch (lc) {
            '_' => '-',
            ' ' => continue,
            else => lc,
        };
        // Copy the normalized character.
        buf[j] = outc;
        j += 1;
    }
    // Slice the buffer to the actual length we used.
    return buf[0..j];
}

// Check if dnf knows about this package
fn dnfPackageExists(a: std.mem.Allocator, out: anytype, pkg: []const u8) !bool {
    // Run `dnf info -q` and use its exit code to decide.
    const res = try runCmdAlloc(a, &.{ "dnf", "info", "-q", pkg });
    defer a.free(res.stdout);
    defer a.free(res.stderr);

    // Check exit code and stderr for “No match for argument”
    return switch (res.term) {
        .Exited => |code| blk: {
            // Exit code 0 means the package exists.
            if (code == 0) break :blk true;

            if (res.stderr.len != 0) {
                // If it is not "No match", show the error to the user.
                if (!std.mem.containsAtLeast(u8, res.stderr, 1, "No match for argument")) {
                    try out.print("dnf info error for {s}:\n{s}\n", .{ pkg, res.stderr });
                }
            }
            break :blk false;
        },
        else => false,
    };
}
