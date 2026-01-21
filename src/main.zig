// GOAL (target vision)
// This tool helps users migrate from an immutable Fedora-like OSTree system (e.g. rpm-ostree based)
// to a mutable Fedora-like system.
//
// Backup mode (on immutable):
//   - Saves a timestamped backup folder under ./backups/backup-<unix_ts>/
//   - Captures:
//       1) flatpak-list.txt     (app-id, origin, branch)
//       2) rpm-ostree-status.json (raw JSON, but restore will extract only "layered/extra" packages)
//
// Restore mode (on mutable Fedora):
//   - Lets the user pick a backup folder
//   - Restores "extra layered RPMs" by installing them via dnf
//   - Converts Flatpak app IDs to RPM packages in a safe, user-controlled way:
//       - Try exact / strongly-derived candidates first
//       - If multiple plausible RPM candidates exist, the user chooses a number in the terminal
//       - If no candidate is found, prints a clear message to install manually
//
// Design principle:
//   - No fuzzy auto-install that could install the wrong thing without user consent.
//   - If ambiguous: ask. If unknown: report and skip.

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

    // assign a value to kind based on the two booleans
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

    // assign another value to supported based on kind, or exit if unsupported
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
    var stdin_buffer: [256]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const in = &stdin_reader.interface;

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
                // NEW: restore is now implemented and interactive
                try runRestore(out, in);
                try out.print("Restore flow completed.\n", .{});
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

    // parse lines
    var found_id: ?[]const u8 = null;
    var found_id_like: ?[]const u8 = null;

    // read lines
    while (true) {
        const maybe_line = try r.takeDelimiter('\n');
        if (maybe_line == null) break;

        const line = std.mem.trim(u8, maybe_line.?, " \t\r\n");
        if (line.len == 0) continue;

        if (found_id == null) {
            if (parseOsReleaseValue(line, "ID")) |v| found_id = v;
        }

        if (found_id_like == null) {
            if (parseOsReleaseValue(line, "ID_LIKE")) |v| found_id_like = v;
        }

        if (found_id != null and found_id_like != null) break;
    }

    // check for fedora
    if (found_id) |id| {
        if (std.mem.eql(u8, id, "fedora")) return true;
    }

    // check ID_LIKE
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

// parse user action char based on supported system kind
const Action = enum { backup, restore };

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

// NEW: richer command result (stdout + stderr + termination info)
const CmdResult = struct {
    stdout: []u8,
    stderr: []u8,
    term: std.process.Child.Term,
};

// run command and capture stdout + stderr
fn runCmdAlloc(allocator: std.mem.Allocator, argv: []const []const u8) !CmdResult {
    var child = std.process.Child.init(argv, allocator);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    const stdout_bytes = try child.stdout.?.readToEndAlloc(allocator, 1024 * 1024 * 8); // 8MB cap
    const stderr_bytes = try child.stderr.?.readToEndAlloc(allocator, 1024 * 1024 * 2); // 2MB cap
    const term = try child.wait();

    return .{ .stdout = stdout_bytes, .stderr = stderr_bytes, .term = term };
}

// CHANGED: enforce success so you don’t create “valid-looking but incomplete” backups/restores
fn runCmdCheckedAlloc(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    out: anytype,
) ![]u8 {
    const res = try runCmdAlloc(allocator, argv);

    // check exit code
    switch (res.term) {
        .Exited => |code| {
            if (code != 0) {
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
            try out.print("Command did not exit normally:", .{});
            for (argv) |arg| try out.print(" {s}", .{arg});
            try out.print("\n", .{});
            if (res.stderr.len != 0) {
                try out.print("stderr:\n{s}\n", .{res.stderr});
            }
            return error.CommandFailed;
        },
    }

    return res.stdout;
}

// write data to file at relative path (from cwd)
fn writeFileRel(path: []const u8, data: []const u8) !void {
    var f = try std.fs.cwd().createFile(path, .{ .truncate = true });
    defer f.close();
    try f.writeAll(data);
}

// ---------------------BACKUP---------------------

// main backup driver
fn runBackup(out: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const ts = std.time.timestamp();
    const dir_name = try std.fmt.allocPrint(a, "backups/backup-{d}", .{ts});

    std.fs.cwd().makePath("backups") catch |err| {
        if (err != error.PathAlreadyExists) return err;
    };

    std.fs.cwd().makePath(dir_name) catch |err| {
        if (err != error.PathAlreadyExists) return err;
    };

    try out.print("Backup directory created at: {s}\n", .{dir_name});

    // CHANGED: flatpak list includes origin + branch (stable/beta)
    const flatpak_out = try runCmdCheckedAlloc(a, &.{
        "flatpak",
        "list",
        "--app",
        "--columns=application,origin,branch",
    }, out);

    // write flatpak-list.txt
    const flatpak_path = try std.fmt.allocPrint(a, "{s}/flatpak-list.txt", .{dir_name});
    try writeFileRel(flatpak_path, flatpak_out);
    try out.print("Wrote {s}\n", .{flatpak_path});

    // Keep the raw JSON. Restore will extract only what it needs.
    const ostree_out = try runCmdCheckedAlloc(a, &.{
        "rpm-ostree",
        "status",
        "--json",
    }, out);

    // write rpm-ostree-status.json
    const ostree_path = try std.fmt.allocPrint(a, "{s}/rpm-ostree-status.json", .{dir_name});
    try writeFileRel(ostree_path, ostree_out);
    try out.print("Wrote {s}\n", .{ostree_path});

    try out.print("Backup done.\n", .{});
}

// ---------------------RESTORE (NEW)---------------------

// NEW: Represents a row from flatpak-list.txt
const FlatpakEntry = struct {
    app_id: []const u8,
    origin: []const u8,
    branch: []const u8,
};

// NEW: main restore driver
fn runRestore(out: anytype, in: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    // 1) pick backup folder
    const backup_dir = try pickBackupDir(a, out, in);

    // 2) extract layered RPMs from rpm-ostree-status.json and install via dnf
    const layered = try loadLayeredRpmsFromStatusJson(a, out, backup_dir);
    if (layered.len != 0) {
        try out.print("\nLayered RPMs found in backup ({d}):\n", .{layered.len});
        for (layered) |p| try out.print("  - {s}\n", .{p});
        try out.print("\nInstalling layered RPMs via dnf...\n", .{});
        try dnfInstallPkgs(a, out, layered);
        try out.print("Layered RPM install done.\n", .{});
    } else {
        try out.print("\nNo layered RPMs found in backup.\n", .{});
    }

    // 3) read flatpak list and attempt conversion to RPMs, interactive on ambiguity
    const flatpaks = try loadFlatpakList(a, out, backup_dir);
    if (flatpaks.len == 0) {
        try out.print("\nNo Flatpaks found in backup.\n", .{});
        return;
    }

    try out.print("\nFlatpaks found in backup ({d}). Converting to RPM candidates...\n", .{flatpaks.len});

    // We'll accumulate RPMs to install (to run dnf fewer times)
    var rpm_to_install = std.ArrayList([]const u8).init(a);
    defer rpm_to_install.deinit();

    for (flatpaks) |fp| {
        const chosen = try resolveFlatpakToRpmInteractive(a, out, in, fp);
        if (chosen) |pkg_name| {
            try rpm_to_install.append(pkg_name);
        } else {
            try out.print(
                "Skipping (manual install needed): {s} (origin={s}, branch={s})\n",
                .{ fp.app_id, fp.origin, fp.branch },
            );
        }
    }

    if (rpm_to_install.items.len != 0) {
        try out.print("\nInstalling selected RPMs ({d}) via dnf...\n", .{rpm_to_install.items.len});
        try dnfInstallPkgs(a, out, rpm_to_install.items);
        try out.print("RPM install done.\n", .{});
    } else {
        try out.print("\nNo RPMs selected for installation from Flatpak conversion.\n", .{});
    }
}

// NEW: list ./backups/backup-* and let user pick
fn pickBackupDir(a: std.mem.Allocator, out: anytype, in: anytype) ![]const u8 {
    var backups_dir = std.fs.cwd().openDir("backups", .{ .iterate = true }) catch |err| {
        if (err == error.FileNotFound) {
            try out.print("No ./backups directory found. Nothing to restore.\n", .{});
            return error.NoBackups;
        }
        return err;
    };
    defer backups_dir.close();

    // gather backup-* directories
    var it = backups_dir.iterate();
    var names = std.ArrayList([]const u8).init(a);
    defer names.deinit();

    // iterate entries
    while (try it.next()) |entry| {
        if (entry.kind != .directory) continue;
        if (!std.mem.startsWith(u8, entry.name, "backup-")) continue;

        // Copy name into arena-owned memory
        const copy = try a.dupe(u8, entry.name);
        try names.append(copy);
    }

    if (names.items.len == 0) {
        try out.print("No backup-* directories found under ./backups.\n", .{});
        return error.NoBackups;
    }

    // Simple sort: newest first by lexicographic, works because "backup-<timestamp>"
    std.mem.sort([]const u8, names.items, {}, struct {
        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
            // reverse order
            return std.mem.order(u8, lhs, rhs) == .gt;
        }
    }.lessThan);

    try out.print("\nAvailable backups:\n", .{});
    for (names.items, 0..) |n, i| {
        try out.print("  {d}) {s}\n", .{ i + 1, n });
    }

    while (true) {
        try out.print("Choose a backup number: ", .{});
        try out.flush();

        const maybe_line = try in.takeDelimiter('\n');
        if (maybe_line == null) return error.EOF;

        const line = std.mem.trim(u8, maybe_line.?, " \t\r\n");
        if (line.len == 0) continue;

        const idx = std.fmt.parseInt(usize, line, 10) catch {
            try out.print("Please enter a number.\n", .{});
            continue;
        };

        if (idx == 0 or idx > names.items.len) {
            try out.print("Out of range.\n", .{});
            continue;
        }

        const chosen_name = names.items[idx - 1];
        return try std.fmt.allocPrint(a, "backups/{s}", .{chosen_name});
    }
}

// NEW: Load and parse flatpak-list.txt (tab-separated columns)
fn loadFlatpakList(a: std.mem.Allocator, out: anytype, backup_dir: []const u8) ![]FlatpakEntry {
    const path = try std.fmt.allocPrint(a, "{s}/flatpak-list.txt", .{backup_dir});
    var f = std.fs.cwd().openFile(path, .{}) catch |err| {
        if (err == error.FileNotFound) {
            try out.print("No flatpak-list.txt in {s}\n", .{backup_dir});
            return &[_]FlatpakEntry{};
        }
        return err;
    };
    defer f.close();

    // read all data
    const data = try f.readToEndAlloc(a, 1024 * 1024 * 2); // 2MB cap
    var lines = std.mem.splitScalar(u8, data, '\n');

    var entries = std.ArrayList(FlatpakEntry).init(a);
    defer entries.deinit();

    // parse lines
    while (lines.next()) |raw| {
        const line = std.mem.trim(u8, raw, " \t\r\n");
        if (line.len == 0) continue;

        // expected: app_id \t origin \t branch
        var cols = std.mem.tokenizeScalar(u8, line, '\t');
        const app_id = cols.next() orelse continue;
        const origin = cols.next() orelse "";
        const branch = cols.next() orelse "";

        try entries.append(.{
            .app_id = app_id,
            .origin = origin,
            .branch = branch,
        });
    }

    return try entries.toOwnedSlice();
}

// NEW: Extract only "layered/extra packages" from the rpm-ostree status JSON.
// This intentionally ignores removals/overrides (per your requirement).
fn loadLayeredRpmsFromStatusJson(
    a: std.mem.Allocator,
    out: anytype,
    backup_dir: []const u8,
) ![][]const u8 {
    const path = try std.fmt.allocPrint(a, "{s}/rpm-ostree-status.json", .{backup_dir});
    var f = std.fs.cwd().openFile(path, .{}) catch |err| {
        if (err == error.FileNotFound) {
            try out.print("No rpm-ostree-status.json in {s}\n", .{backup_dir});
            return &[_][]const u8{};
        }
        return err;
    };
    defer f.close();

    const data = try f.readToEndAlloc(a, 1024 * 1024 * 16); // 16MB cap
    var parsed = try std.json.parseFromSlice(std.json.Value, a, data, .{});
    defer parsed.deinit();

    const root = parsed.value;

    // Find deployments array
    const deployments_v = root.object.get("deployments") orelse return &[_][]const u8{};
    if (deployments_v != .array) return &[_][]const u8{};

    // Choose booted deployment if possible; otherwise use first
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
    if (chosen == null and deployments_v.array.items.len != 0) {
        chosen = deployments_v.array.items[0];
    }
    if (chosen == null) return &[_][]const u8{};

    const dep = chosen.?;
    if (dep != .object) return &[_][]const u8{};

    // CHANGED: We only look for keys that typically represent user-requested extra packages.
    // Different rpm-ostree builds may expose different keys; we check a few common ones.
    // We do NOT use removals/overrides by design.
    const keys = [_][]const u8{
        "requested-packages", // common
        "packages", // sometimes contains layered
        "requested-base-packages", // sometimes present
        "requested-local-packages", // already seen in your sample (likely empty)
    };

    // Collect unique package names
    var pkgs = std.StringHashMap(void).init(a);
    defer pkgs.deinit();

    // iterate keys
    for (keys) |k| {
        if (dep.object.get(k)) |v| {
            if (v == .array) {
                for (v.array.items) |item| {
                    if (item == .string) {
                        const s = std.mem.trim(u8, item.string, " \t\r\n");
                        if (s.len == 0) continue;
                        // store unique
                        _ = try pkgs.put(s, {});
                    }
                }
            }
        }
    }

    // Convert map keys to slice
    var out_list = std.ArrayList([]const u8).init(a);
    defer out_list.deinit();

    var it = pkgs.iterator();
    while (it.next()) |kv| {
        try out_list.append(kv.key_ptr.*);
    }

    // Sort for stable output
    std.mem.sort([]const u8, out_list.items, {}, struct {
        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
            return std.mem.order(u8, lhs, rhs) == .lt;
        }
    }.lessThan);

    return try out_list.toOwnedSlice();
}

// NEW: Install packages via dnf in one command
fn dnfInstallPkgs(a: std.mem.Allocator, out: anytype, pkgs: []const []const u8) !void {
    if (pkgs.len == 0) return;

    // build argv
    var argv = std.ArrayList([]const u8).init(a);
    defer argv.deinit();

    // dnf install -y pkg1 pkg2 ...
    try argv.append("dnf");
    try argv.append("install");
    try argv.append("-y");

    for (pkgs) |p| try argv.append(p);

    _ = try runCmdCheckedAlloc(a, argv.items, out);
}

// ---------------------FLATPAK -> RPM RESOLUTION (NEW)---------------------

// NEW: Attempt to resolve one Flatpak app-id to an RPM package name.
// If multiple candidates exist, ask the user to choose a number.
fn resolveFlatpakToRpmInteractive(
    a: std.mem.Allocator,
    out: anytype,
    in: anytype,
    fp: FlatpakEntry,
) !?[]const u8 {
    try out.print("\nFlatpak: {s} (origin={s}, branch={s})\n", .{ fp.app_id, fp.origin, fp.branch });

    // 1) generate candidate RPM names (strong derivations only, no fuzzy search)
    const candidates = try deriveRpmCandidates(a, fp.app_id);

    // 2) filter to existing packages by probing dnf
    var existing = std.ArrayList([]const u8).init(a);
    defer existing.deinit();

    for (candidates) |cand| {
        if (try dnfPackageExists(a, out, cand)) {
            try existing.append(cand);
        }
    }

    // Deduplicate (deriveRpmCandidates already tries, but keep safe)
    var uniq = std.StringHashMap(void).init(a);
    defer uniq.deinit();

    // collect unique existing
    var unique_existing = std.ArrayList([]const u8).init(a);
    defer unique_existing.deinit();

    // deduplicate existing
    for (existing.items) |p| {
        if (!uniq.contains(p)) {
            _ = try uniq.put(p, {});
            try unique_existing.append(p);
        }
    }

    // Analyze results
    if (unique_existing.items.len == 0) {
        try out.print("No unambiguous RPM candidate found.\n", .{});
        try out.print("Action: please install manually.\n", .{});
        return null;
    }

    // 2) single option: accept
    if (unique_existing.items.len == 1) {
        const chosen = unique_existing.items[0];
        try out.print("Resolved uniquely -> RPM: {s}\n", .{chosen});
        return chosen;
    }

    // 3) multiple options: ask user to choose
    try out.print("Multiple RPM candidates found. Choose one:\n", .{});
    for (unique_existing.items, 0..) |p, i| {
        try out.print("  {d}) {s}\n", .{ i + 1, p });
    }
    try out.print("  0) skip (manual install)\n", .{});

    // prompt loop
    while (true) {
        try out.print("Your choice: ", .{});
        try out.flush();

        const maybe_line = try in.takeDelimiter('\n');
        if (maybe_line == null) return error.EOF;

        const line = std.mem.trim(u8, maybe_line.?, " \t\r\n");
        if (line.len == 0) continue;

        const n = std.fmt.parseInt(usize, line, 10) catch {
            try out.print("Please enter a number.\n", .{});
            continue;
        };

        if (n == 0) {
            return null;
        }
        if (n > unique_existing.items.len) {
            try out.print("Out of range.\n", .{});
            continue;
        }

        const chosen = unique_existing.items[n - 1];
        try out.print("Chosen -> RPM: {s}\n", .{chosen});
        return chosen;
    }
}

// NEW: Derive strong candidate RPM names from a Flatpak app id.
// This avoids fuzzy searching and keeps decisions safe.
// Example: com.visualstudio.code -> ["code", "visualstudio-code", "vscode", ...] (limited)
fn deriveRpmCandidates(a: std.mem.Allocator, app_id: []const u8) ![]const []const u8 {
    // last component after last '.'
    const last_dot = std.mem.lastIndexOfScalar(u8, app_id, '.') orelse 0;
    const last = if (last_dot == 0) app_id else app_id[(last_dot + 1)..];

    // Normalize: lowercase, '_' -> '-', remove spaces (shouldn't exist)
    const norm_last = try normalizeToken(a, last);

    // Also consider the full tail with dots replaced by dashes, e.g. github.* -> github-*
    const norm_full = try normalizeToken(a, app_id);

    // Some conservative variants (kept small on purpose)
    var list = std.ArrayList([]const u8).init(a);
    defer list.deinit();

    try list.append(norm_last);

    // If last contains '-' already, also try stripping common prefixes/suffixes lightly
    // (Still "strong derivation", not fuzzy.)
    if (std.mem.endsWith(u8, norm_last, "-client")) {
        try list.append(norm_last[0 .. norm_last.len - "-client".len]);
    }
    if (std.mem.endsWith(u8, norm_last, "-desktop")) {
        try list.append(norm_last[0 .. norm_last.len - "-desktop".len]);
    }

    // Sometimes app id tail is like "teams_for_linux" -> "teams-for-linux" (already handled)
    // Provide one more candidate: full id with dashes, but that is often too long.
    // Still cheap to probe.
    try list.append(norm_full);

    // Provide a very conservative "reverse-dns removed but keep vendor + product" candidate:
    // take last two segments if available
    var segs = std.ArrayList([]const u8).init(a);
    defer segs.deinit();

    // split by '.'
    var it = std.mem.splitScalar(u8, app_id, '.');
    while (it.next()) |seg| {
        if (seg.len == 0) continue;
        try segs.append(seg);
    }

    // if at least two segments, form vendor-product
    if (segs.items.len >= 2) {
        const vendor = try normalizeToken(a, segs.items[segs.items.len - 2]);
        const product = try normalizeToken(a, segs.items[segs.items.len - 1]);
        const combo = try std.fmt.allocPrint(a, "{s}-{s}", .{ vendor, product });
        try list.append(combo);
    }

    // Deduplicate
    var uniq = std.StringHashMap(void).init(a);
    defer uniq.deinit();

    var out_list = std.ArrayList([]const u8).init(a);
    defer out_list.deinit();

    // collect unique
    for (list.items) |s| {
        if (s.len == 0) continue;
        if (!uniq.contains(s)) {
            _ = try uniq.put(s, {});
            try out_list.append(s);
        }
    }

    return try out_list.toOwnedSlice();
}

// NEW: normalize token for RPM candidate checks
fn normalizeToken(a: std.mem.Allocator, s: []const u8) ![]const u8 {
    var buf = try a.alloc(u8, s.len);
    var j: usize = 0;
    for (s) |c| {
        const lc = std.ascii.toLower(c);
        const outc: u8 = switch (lc) {
            '_' => '-',
            ' ' => continue,
            else => lc,
        };
        buf[j] = outc;
        j += 1;
    }
    return buf[0..j];
}

// NEW: check if dnf knows an exact package name (no fuzzy search)
// We use `dnf info -q <name>` as a simple existence probe.
fn dnfPackageExists(a: std.mem.Allocator, out: anytype, pkg: []const u8) !bool {
    const res = try runCmdAlloc(a, &.{ "dnf", "info", "-q", pkg });

    // Exited code 0 -> exists, else -> doesn't exist (or dnf issue)
    return switch (res.term) {
        .Exited => |code| blk: {
            if (code == 0) break :blk true;

            // If dnf itself failed hard, show stderr once for visibility
            // (We keep it quiet for normal "not found".)
            if (code != 0 and res.stderr.len != 0) {
                // Heuristic: only print if it looks like a real error, not "No match for argument"
                if (!std.mem.containsAtLeast(u8, res.stderr, 1, "No match for argument")) {
                    try out.print("dnf info error for {s}:\n{s}\n", .{ pkg, res.stderr });
                }
            }
            break :blk false;
        },
        else => false,
    };
}
