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

const std = @import("std");

// Zig 0.15.x: std.ArrayList(T) is unmanaged by default.
// If you want the classic API (.init(allocator), .append, .deinit, .toOwnedSlice()),
// use std.array_list.Managed(T).
const ArrayListManaged = std.array_list.Managed;

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

const Action = enum { backup, restore };

// Represents a row from flatpak-list.txt
const FlatpakEntry = struct {
    app_id: []const u8,
    origin: []const u8,
    branch: []const u8,
};

// Command result (stdout + stderr + termination info)
const CmdResult = struct {
    stdout: []u8,
    stderr: []u8,
    term: std.process.Child.Term,
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
        if (err == error.FileNotFound) return false;
        return err;
    };
    defer file.close();
    return true;
}

// helper to parse KEY=value or KEY="value" lines from /etc/os-release
fn parseOsReleaseValue(line: []const u8, key: []const u8) ?[]const u8 {
    if (!std.mem.startsWith(u8, line, key)) return null;
    if (line.len <= key.len or line[key.len] != '=') return null;

    const raw = line[(key.len + 1)..];
    return std.mem.trim(u8, raw, " \t\r\n\"'");
}

// returns true if ID == "fedora" OR ID_LIKE contains "fedora"
fn isFedoraLikeSystem() !bool {
    const os_release_path = "/etc/os-release";

    var file = std.fs.openFileAbsolute(os_release_path, .{}) catch |err| {
        if (err == error.FileNotFound) return false;
        return err;
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
            if (parseOsReleaseValue(line, "ID")) |v| found_id = v;
        }
        if (found_id_like == null) {
            if (parseOsReleaseValue(line, "ID_LIKE")) |v| found_id_like = v;
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

// enforce success so you don’t create “valid-looking but incomplete” backups/restores
fn runCmdCheckedAlloc(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    out: anytype,
) ![]u8 {
    const res = try runCmdAlloc(allocator, argv);
    errdefer allocator.free(res.stdout);
    errdefer allocator.free(res.stderr);

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

    allocator.free(res.stderr);
    return res.stdout;
}

fn writeFileRel(path: []const u8, data: []const u8) !void {
    var f = try std.fs.cwd().createFile(path, .{ .truncate = true });
    defer f.close();
    try f.writeAll(data);
}

fn writeLinesRel(path: []const u8, lines: []const []const u8) !void {
    // NEW: helper for writing small “relevant output” files (one item per line).
    var f = try std.fs.cwd().createFile(path, .{ .truncate = true }); // CHANGED: new helper
    defer f.close();

    for (lines) |line| {
        try f.writeAll(line);
        try f.writeAll("\n");
    }
}

// ---------------------BACKUP HELPERS---------------------

fn getHomeDir(a: std.mem.Allocator) ![]const u8 {
    return try std.process.getEnvVarOwned(a, "HOME");
}

// Parse the flatpak-list output: tab-separated columns
fn parseFlatpakListFromBytes(a: std.mem.Allocator, data: []const u8) ![]FlatpakEntry {
    var lines = std.mem.splitScalar(u8, data, '\n');

    var entries = ArrayListManaged(FlatpakEntry).init(a);
    defer entries.deinit();

    while (lines.next()) |raw| {
        const line = std.mem.trim(u8, raw, " \t\r\n");
        if (line.len == 0) continue;

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

// Backup: copy ~/.var/app/<app_id>/config -> <backupdir>/flatpak-configs/<app_id>/config
fn backupFlatpakConfigs(
    a: std.mem.Allocator,
    out: anytype,
    backup_dir: []const u8,
    flatpak_list_bytes: []const u8,
) !void {
    const home = try getHomeDir(a);

    const entries = try parseFlatpakListFromBytes(a, flatpak_list_bytes);
    if (entries.len == 0) {
        try out.print("No Flatpaks in list -> skipping Flatpak config backup.\n", .{});
        return;
    }

    const root_rel = try std.fmt.allocPrint(a, "{s}/flatpak-configs", .{backup_dir});
    std.fs.cwd().makePath(root_rel) catch |err| {
        if (err != error.PathAlreadyExists) return err;
    };

    var copied_count: usize = 0;

    for (entries) |fp| {
        const src_abs = try std.fmt.allocPrint(a, "{s}/.var/app/{s}/config", .{ home, fp.app_id });

        // Existence check (dir) WITHOUT leaking the handle
        var src_dir = std.fs.openDirAbsolute(src_abs, .{}) catch |err| {
            if (err == error.FileNotFound) continue; // no config dir -> skip
            return err;
        };
        src_dir.close();

        const dst_rel = try std.fmt.allocPrint(a, "{s}/{s}", .{ root_rel, fp.app_id });
        std.fs.cwd().makePath(dst_rel) catch |err| {
            if (err != error.PathAlreadyExists) return err;
        };

        const mk_out = try runCmdCheckedAlloc(a, &.{ "mkdir", "-p", dst_rel }, out);
        a.free(mk_out);

        // cp -a <src_abs> <dst_rel>/
        // This creates: <dst_rel>/config
        const cp_out = try runCmdCheckedAlloc(a, &.{ "cp", "-a", src_abs, dst_rel }, out);
        a.free(cp_out);

        copied_count += 1;
        try out.print("Backed up Flatpak config: {s} -> {s}/\n", .{ src_abs, dst_rel });
    }

    try out.print("Flatpak config backup done. Copied {d} config dirs.\n", .{copied_count});
}

fn backupDotConfig(
    a: std.mem.Allocator,
    out: anytype,
    backup_dir: []const u8,
) !void {
    // NEW: backup of ~/.config (general app configs, not only Flatpak).
    // We store it as "<backup>/dot-config/" so restore can merge it into ~/.config/.
    const home = try getHomeDir(a);
    const src_abs = try std.fmt.allocPrint(a, "{s}/.config", .{home});
    const dst_rel = try std.fmt.allocPrint(a, "{s}/dot-config", .{backup_dir}); // CHANGED/NEW

    // Ensure destination exists.
    const mk_out = try runCmdCheckedAlloc(a, &.{ "mkdir", "-p", dst_rel }, out); // NEW
    a.free(mk_out);

    // Copy the *contents* of ~/.config into dot-config:
    // Using "/." copies contents rather than nesting the directory.
    const src_with_dot = try std.fmt.allocPrint(a, "{s}/.", .{src_abs}); // NEW
    const cp_out = try runCmdCheckedAlloc(a, &.{ "cp", "-a", src_with_dot, dst_rel }, out); // NEW
    a.free(cp_out);

    try out.print("Backed up ~/.config -> {s}\n", .{dst_rel}); // NEW
}

fn extractLayeredRpmsFromOstreeJsonBytes(
    a: std.mem.Allocator,
    json_bytes: []const u8,
) ![][]const u8 {
    // NEW: shared helper so backup can create a minimal layered-rpms.txt
    var parsed = try std.json.parseFromSlice(std.json.Value, a, json_bytes, .{}); // NEW
    defer parsed.deinit();

    const root = parsed.value;

    const deployments_v = root.object.get("deployments") orelse return &[_][]const u8{};
    if (deployments_v != .array) return &[_][]const u8{};

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

    // These keys are where rpm-ostree tends to store “requested/layered” packages.
    const keys = [_][]const u8{
        "requested-packages",
        "packages",
        "requested-base-packages",
        "requested-local-packages",
    };

    var pkgs = std.StringHashMap(void).init(a);
    defer pkgs.deinit();

    for (keys) |k| {
        if (dep.object.get(k)) |v| {
            if (v == .array) {
                for (v.array.items) |item| {
                    if (item == .string) {
                        const s = std.mem.trim(u8, item.string, " \t\r\n");
                        if (s.len == 0) continue;
                        _ = try pkgs.put(s, {});
                    }
                }
            }
        }
    }

    var out_list = ArrayListManaged([]const u8).init(a);
    defer out_list.deinit();

    var it = pkgs.iterator();
    while (it.next()) |kv| {
        try out_list.append(kv.key_ptr.*);
    }

    std.mem.sort([]const u8, out_list.items, {}, struct {
        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
            return std.mem.order(u8, lhs, rhs) == .lt;
        }
    }.lessThan);

    return try out_list.toOwnedSlice();
}

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

    const flatpak_out = try runCmdCheckedAlloc(a, &.{
        "flatpak",
        "list",
        "--app",
        "--columns=application,origin,branch",
    }, out);

    const flatpak_path = try std.fmt.allocPrint(a, "{s}/flatpak-list.txt", .{dir_name});
    try writeFileRel(flatpak_path, flatpak_out);
    try out.print("Wrote {s}\n", .{flatpak_path});

    // Backup Flatpak configs
    try backupFlatpakConfigs(a, out, dir_name, flatpak_out);

    // NEW: Backup ~/.config (general non-Flatpak configs)
    try backupDotConfig(a, out, dir_name); // NEW

    // CHANGED: We still call rpm-ostree status --json, but we do NOT store the huge JSON anymore.
    // Instead we extract only the layered/requested packages and write layered-rpms.txt.
    const ostree_json = try runCmdCheckedAlloc(a, &.{
        "rpm-ostree",
        "status",
        "--json",
    }, out); // CHANGED: capture JSON only as input for extraction

    const layered = try extractLayeredRpmsFromOstreeJsonBytes(a, ostree_json); // NEW
    const layered_path = try std.fmt.allocPrint(a, "{s}/layered-rpms.txt", .{dir_name}); // NEW
    try writeLinesRel(layered_path, layered); // NEW: minimal “relevant output”
    try out.print("Wrote {s} ({d} packages)\n", .{ layered_path, layered.len }); // NEW

    try out.print("Backup done.\n", .{});
}

// ---------------------RESTORE---------------------

fn runRestore(out: anytype, in: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const backup_dir = try pickBackupDir(a, out, in);

    // CHANGED: prefer the minimal layered-rpms.txt; fallback to old rpm-ostree-status.json.
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

    const flatpaks = try loadFlatpakList(a, out, backup_dir);
    if (flatpaks.len == 0) {
        try out.print("\nNo Flatpaks found in backup.\n", .{});
        // Even if no Flatpaks, we still restore ~/.config because it’s useful.
        try out.print("\nRestoring ~/.config (dot-config) into ~/.config/...\n", .{}); // NEW
        try restoreDotConfig(a, out, backup_dir); // NEW
        return;
    }

    try out.print("\nFlatpaks found in backup ({d}). Converting to RPM candidates...\n", .{flatpaks.len});

    var rpm_to_install = ArrayListManaged([]const u8).init(a);
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

    // Restore Flatpak configs
    try out.print("\nRestoring Flatpak configs into ~/.config/...\n", .{});
    try restoreFlatpakConfigs(a, out, backup_dir);

    // NEW: Restore general ~/.config backup (merge)
    try out.print("\nRestoring ~/.config (dot-config) into ~/.config/...\n", .{}); // NEW
    try restoreDotConfig(a, out, backup_dir); // NEW
}

// list ./backups/backup-* and let user pick
fn pickBackupDir(a: std.mem.Allocator, out: anytype, in: anytype) ![]const u8 {
    var backups_dir = std.fs.cwd().openDir("backups", .{ .iterate = true }) catch |err| {
        if (err == error.FileNotFound) {
            try out.print("No ./backups directory found. Nothing to restore.\n", .{});
            return error.NoBackups;
        }
        return err;
    };
    defer backups_dir.close();

    var it = backups_dir.iterate();
    var names = ArrayListManaged([]const u8).init(a);
    defer names.deinit();

    while (try it.next()) |entry| {
        if (entry.kind != .directory) continue;
        if (!std.mem.startsWith(u8, entry.name, "backup-")) continue;

        const copy = try a.dupe(u8, entry.name);
        try names.append(copy);
    }

    if (names.items.len == 0) {
        try out.print("No backup-* directories found under ./backups.\n", .{});
        return error.NoBackups;
    }

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

// ---------------------LAYERED RPM LOADING---------------------

fn loadLayeredRpms(a: std.mem.Allocator, out: anytype, backup_dir: []const u8) ![][]const u8 {
    // CHANGED: new main loader: prefer layered-rpms.txt for minimal relevant data.
    if (try loadLayeredRpmsFromTxt(a, out, backup_dir)) |pkgs| {
        return pkgs;
    }

    // Fallback for older backups that still have rpm-ostree-status.json.
    return try loadLayeredRpmsFromStatusJson(a, out, backup_dir); // CHANGED
}

//  load layered-rpms.txt (one package name per line)
fn loadLayeredRpmsFromTxt(a: std.mem.Allocator, out: anytype, backup_dir: []const u8) !?[][]const u8 {
    // NEW: reads "<backup>/layered-rpms.txt" (one package per line)
    const path = try std.fmt.allocPrint(a, "{s}/layered-rpms.txt", .{backup_dir});
    var f = std.fs.cwd().openFile(path, .{}) catch |err| {
        if (err == error.FileNotFound) return null; // NEW: signal “use fallback”
        return err;
    };
    defer f.close();

    // Read and parse lines
    const data = try f.readToEndAlloc(a, 1024 * 1024); // 1MB cap; should be tiny
    var lines = std.mem.splitScalar(u8, data, '\n');

    // Collect package names
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
    const path = try std.fmt.allocPrint(a, "{s}/flatpak-list.txt", .{backup_dir});
    var f = std.fs.cwd().openFile(path, .{}) catch |err| {
        if (err == error.FileNotFound) {
            try out.print("No flatpak-list.txt in {s}\n", .{backup_dir});
            return &[_]FlatpakEntry{};
        }
        return err;
    };
    defer f.close();

    const data = try f.readToEndAlloc(a, 1024 * 1024 * 2); // 2MB cap
    return try parseFlatpakListFromBytes(a, data);
}

// ---------------------OLD JSON FALLBACK (KEEP FOR COMPAT)---------------------

// Extract only "layered/extra packages" from rpm-ostree status JSON.
// Ignores removals/overrides by design.
fn loadLayeredRpmsFromStatusJson(
    a: std.mem.Allocator,
    out: anytype,
    backup_dir: []const u8,
) ![][]const u8 {
    // CHANGED: renamed/kept as fallback for older backups.
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
    const data = try f.readToEndAlloc(a, 1024 * 1024 * 16); // 16MB cap
    var parsed = try std.json.parseFromSlice(std.json.Value, a, data, .{});
    defer parsed.deinit();

    const root = parsed.value;

    const deployments_v = root.object.get("deployments") orelse return &[_][]const u8{};
    if (deployments_v != .array) return &[_][]const u8{};

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

    const keys = [_][]const u8{
        "requested-packages",
        "packages",
        "requested-base-packages",
        "requested-local-packages",
    };

    var pkgs = std.StringHashMap(void).init(a);
    defer pkgs.deinit();

    for (keys) |k| {
        if (dep.object.get(k)) |v| {
            if (v == .array) {
                for (v.array.items) |item| {
                    if (item == .string) {
                        const s = std.mem.trim(u8, item.string, " \t\r\n");
                        if (s.len == 0) continue;
                        _ = try pkgs.put(s, {});
                    }
                }
            }
        }
    }

    var out_list = ArrayListManaged([]const u8).init(a);
    defer out_list.deinit();

    var it = pkgs.iterator();
    while (it.next()) |kv| {
        try out_list.append(kv.key_ptr.*);
    }

    std.mem.sort([]const u8, out_list.items, {}, struct {
        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
            return std.mem.order(u8, lhs, rhs) == .lt;
        }
    }.lessThan);

    try out.print("Loaded layered RPM list from rpm-ostree-status.json (fallback)\n", .{}); // NEW
    return try out_list.toOwnedSlice();
}

fn dnfInstallPkgs(a: std.mem.Allocator, out: anytype, pkgs: []const []const u8) !void {
    if (pkgs.len == 0) return;

    var argv = ArrayListManaged([]const u8).init(a);
    defer argv.deinit();

    try argv.append("dnf");
    try argv.append("install");
    try argv.append("-y");
    for (pkgs) |p| try argv.append(p);

    const cmd_out = try runCmdCheckedAlloc(a, argv.items, out);
    a.free(cmd_out);
}

// ----------------------DOT-CONFIG RESTORE---------------------

fn restoreDotConfig(a: std.mem.Allocator, out: anytype, backup_dir: []const u8) !void {
    // NEW: restore "<backup>/dot-config/" into "~/.config/" as a merge.
    // This is intentionally “copy-on-top”, because ~/.config already exists on the target system.
    const home = try getHomeDir(a);

    const src_abs = try std.fmt.allocPrint(a, "{s}/dot-config", .{backup_dir}); // NEW
    const dst_abs = try std.fmt.allocPrint(a, "{s}/.config", .{home}); // NEW

    // If dot-config doesn't exist in the backup, skip silently.
    var src_dir = std.fs.cwd().openDir(src_abs, .{}) catch |err| {
        if (err == error.FileNotFound) {
            try out.print("No dot-config folder in backup -> skipping ~/.config restore.\n", .{}); // NEW
            return;
        }
        return err;
    };
    src_dir.close();

    // Ensure destination exists (normally it does, but keep it robust).
    const mk_out = try runCmdCheckedAlloc(a, &.{ "mkdir", "-p", dst_abs }, out); // NEW
    a.free(mk_out);

    // Copy the *contents* of dot-config into ~/.config.
    const src_with_dot = try std.fmt.allocPrint(a, "{s}/.", .{src_abs}); // NEW
    const cp_out = try runCmdCheckedAlloc(a, &.{ "cp", "-a", src_with_dot, dst_abs }, out); // NEW
    a.free(cp_out);

    try out.print("Restored dot-config -> {s}\n", .{dst_abs}); // NEW
}

// ----------------------FLATPAK CONFIG RESTORE---------------------

fn flatpakAppIdToConfigName(a: std.mem.Allocator, app_id: []const u8) ![]const u8 {
    var segs = ArrayListManaged([]const u8).init(a);
    defer segs.deinit();

    var it = std.mem.splitScalar(u8, app_id, '.');
    while (it.next()) |seg| {
        if (seg.len == 0) continue;
        try segs.append(seg);
    }
    if (segs.items.len == 0) return try a.dupe(u8, app_id);

    const last = segs.items[segs.items.len - 1];
    const chosen = if (std.mem.eql(u8, last, "desktop") and segs.items.len >= 2)
        segs.items[segs.items.len - 2]
    else
        last;

    return try normalizeToken(a, chosen);
}

// Restore: copy <backupdir>/flatpak-configs/<app_id>/config/. -> ~/.config/<name>/
fn restoreFlatpakConfigs(a: std.mem.Allocator, out: anytype, backup_dir: []const u8) !void {
    const home = try getHomeDir(a);

    const src_root_rel = try std.fmt.allocPrint(a, "{s}/flatpak-configs", .{backup_dir});

    var dir = std.fs.cwd().openDir(src_root_rel, .{ .iterate = true }) catch |err| {
        if (err == error.FileNotFound) {
            try out.print("No flatpak-configs folder in backup -> skipping Flatpak config restore.\n", .{});
            return;
        }
        return err;
    };
    defer dir.close();

    var it = dir.iterate();
    var restored_count: usize = 0;

    while (try it.next()) |entry| {
        if (entry.kind != .directory) continue;

        const app_id = entry.name;
        const name = try flatpakAppIdToConfigName(a, app_id);

        const src_abs = try std.fmt.allocPrint(a, "{s}/{s}/config", .{ src_root_rel, app_id });
        const dst_abs = try std.fmt.allocPrint(a, "{s}/.config/{s}", .{ home, name });

        const mk_out = try runCmdCheckedAlloc(a, &.{ "mkdir", "-p", dst_abs }, out);
        a.free(mk_out);

        const src_with_dot = try std.fmt.allocPrint(a, "{s}/.", .{src_abs});
        const cp_out = try runCmdCheckedAlloc(a, &.{ "cp", "-a", src_with_dot, dst_abs }, out);
        a.free(cp_out);

        restored_count += 1;
        try out.print("Restored Flatpak config: {s} -> {s}\n", .{ src_abs, dst_abs });
    }

    try out.print("Flatpak config restore done. Restored {d} config dirs.\n", .{restored_count});
}

// ---------------------FLATPAK -> RPM RESOLUTION---------------------

// Interactive resolution of Flatpak app ID to RPM package name
fn resolveFlatpakToRpmInteractive(
    a: std.mem.Allocator,
    out: anytype,
    in: anytype,
    fp: FlatpakEntry,
) !?[]const u8 {
    try out.print("\nFlatpak: {s} (origin={s}, branch={s})\n", .{ fp.app_id, fp.origin, fp.branch });

    // Derive candidates
    const candidates = try deriveRpmCandidates(a, fp.app_id);

    // Check which candidates exist in dnf
    var existing = ArrayListManaged([]const u8).init(a);
    defer existing.deinit();

    for (candidates) |cand| {
        if (try dnfPackageExists(a, out, cand)) {
            try existing.append(cand);
        }
    }

    // Filter unique existing candidates
    var uniq = std.StringHashMap(void).init(a);
    defer uniq.deinit();

    // Collect unique existing candidates
    var unique_existing = ArrayListManaged([]const u8).init(a);
    defer unique_existing.deinit();

    for (existing.items) |p| {
        if (!uniq.contains(p)) {
            _ = try uniq.put(p, {});
            try unique_existing.append(p);
        }
    }

    // Resolve based on number of unique existing candidates
    if (unique_existing.items.len == 0) {
        try out.print("No unambiguous RPM candidate found.\n", .{});
        try out.print("Action: please install manually.\n", .{});
        return null;
    }

    // Unique match
    if (unique_existing.items.len == 1) {
        const chosen = unique_existing.items[0];
        try out.print("Resolved uniquely -> RPM: {s}\n", .{chosen});
        return chosen;
    }

    // Multiple matches -> ask user
    try out.print("Multiple RPM candidates found. Choose one:\n", .{});
    for (unique_existing.items, 0..) |p, i| {
        try out.print("  {d}) {s}\n", .{ i + 1, p });
    }
    try out.print("  0) skip (manual install)\n", .{});

    // Prompt loop
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

        if (n == 0) return null;
        if (n > unique_existing.items.len) {
            try out.print("Out of range.\n", .{});
            continue;
        }

        const chosen = unique_existing.items[n - 1];
        try out.print("Chosen -> RPM: {s}\n", .{chosen});
        return chosen;
    }
}

// Derive possible RPM package names from Flatpak app ID
fn deriveRpmCandidates(a: std.mem.Allocator, app_id: []const u8) ![]const []const u8 {
    const last_dot = std.mem.lastIndexOfScalar(u8, app_id, '.') orelse 0;
    const last = if (last_dot == 0) app_id else app_id[(last_dot + 1)..];

    const norm_last = try normalizeToken(a, last);
    const norm_full = try normalizeToken(a, app_id);

    var list = ArrayListManaged([]const u8).init(a);
    defer list.deinit();

    try list.append(norm_last);

    if (std.mem.endsWith(u8, norm_last, "-client")) {
        try list.append(norm_last[0 .. norm_last.len - "-client".len]);
    }
    if (std.mem.endsWith(u8, norm_last, "-desktop")) {
        try list.append(norm_last[0 .. norm_last.len - "-desktop".len]);
    }

    try list.append(norm_full);

    // Vendor-product combo
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

    return try out_list.toOwnedSlice();
}

// Normalize token: lowercase, replace '_' with '-', remove spaces
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

// Check if dnf knows about this package
fn dnfPackageExists(a: std.mem.Allocator, out: anytype, pkg: []const u8) !bool {
    const res = try runCmdAlloc(a, &.{ "dnf", "info", "-q", pkg });
    defer a.free(res.stdout);
    defer a.free(res.stderr);

    // Check exit code and stderr for “No match for argument”
    return switch (res.term) {
        .Exited => |code| blk: {
            if (code == 0) break :blk true;

            if (res.stderr.len != 0) {
                if (!std.mem.containsAtLeast(u8, res.stderr, 1, "No match for argument")) {
                    try out.print("dnf info error for {s}:\n{s}\n", .{ pkg, res.stderr });
                }
            }
            break :blk false;
        },
        else => false,
    };
}
