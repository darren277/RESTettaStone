const std = @import("std");
const pg = @import("pg");
const zap = @import("zap");

var pool: ?*pg.Pool = null;

fn getEnvVar(env_map: std.process.EnvMap, key: []const u8, default_value: []const u8) []const u8 {
    if (env_map.get(key)) |value| {
        return value;
    }
    return default_value;
}

fn on_request(r: zap.Request) void {
    if (r.path) |the_path| {
        std.debug.print("PATH: {s}\n", .{the_path});
    }

    if (r.query) |the_query| {
        std.debug.print("QUERY: {s}\n", .{the_query});
    }

    r.setContentType(.JSON) catch return;

    // Use the global pool to run queries
    const users_json = queryUsers() catch |err| {
        std.debug.print("An error occurred: {}\n", .{err});
        r.sendBody("{\"error\": \"An unexpected error occurred\"}") catch return;
        return;
    };

    r.sendBody(users_json) catch return;
}


fn queryUsers() ![]const u8 {
    if (pool == null) {
        return error.PoolNotInitialized;
    }
    var conn = try pool.?.acquire();
    defer conn.release();

    // Allocator to handle temporary memory for query results
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    // Allocator to handle JSON buffer
    var json_buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer json_buf.deinit();
    try json_buf.appendSlice("[");

    var first = true;

    var result = try conn.queryOpts("select * from users order by id", .{}, .{ .allocator = arena.allocator() });
    defer result.deinit();

    while (try result.next()) |row| {
        // Somewhat arbitrary. Not ideal.
        const email_row_index = 2;

        const id = row.get(i32, 0);
        const email = row.get([]const u8, email_row_index);

        if (!first) {
            try json_buf.appendSlice(",");
        }
        first = false;

        // TODO: See if there's a library for parsing JSON
        var id_buf: [20]u8 = undefined;
        const id_str = try std.fmt.bufPrint(&id_buf, "{d}", .{id});
        try json_buf.appendSlice("{\"id\":");
        try json_buf.appendSlice(id_str);
        try json_buf.appendSlice(",\"email\":\"");
        try json_buf.appendSlice(email);
        try json_buf.appendSlice("\"}");
    }

    try json_buf.appendSlice("]");

    return json_buf.toOwnedSlice();
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var env_map = try std.process.getEnvMap(allocator);
    defer env_map.deinit();

    const pg_host = getEnvVar(env_map, "PG_HOST", "localhost");
    const pg_port = std.fmt.parseInt(u16, getEnvVar(env_map, "PG_PORT", "5432"), 10) catch 5432;
    const pg_user = getEnvVar(env_map, "PG_USER", "myusername");
    const pg_pass = getEnvVar(env_map, "PG_PASS", "mypassword");
    const pg_db = getEnvVar(env_map, "PG_DB", "postgres");
    const port = std.fmt.parseInt(u16, getEnvVar(env_map, "PORT", "3000"), 10) catch 3000;

    pool = try pg.Pool.init(allocator, .{
        .size = 5,
        .connect = .{
            .port = pg_port,
            .host = pg_host,
        },
        .auth = .{
            .username = pg_user,
            .database = pg_db,
            .password = pg_pass,
            .timeout = 10_000,
        }
    });

    defer if (pool) |p| p.deinit();

    var listener = zap.HttpListener.init(.{
        .port = port,
        .on_request = on_request,
        .log = true,
    });
    try listener.listen();

    std.debug.print("Listening on 0.0.0.0:{d}\n", .{port});

    zap.start(.{
        .threads = 2,
        .workers = 2,
    });
}
