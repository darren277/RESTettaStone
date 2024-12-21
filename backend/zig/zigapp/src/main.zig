const std = @import("std");
const pg = @import("pg");
const zap = @import("zap");

const Method = std.http.Method;

var pool: ?*pg.Pool = null;

const UserInput = struct {
    email: []const u8,
};

fn getEnvVar(env_map: std.process.EnvMap, key: []const u8, default_value: []const u8) []const u8 {
    if (env_map.get(key)) |value| {
        return value;
    }
    return default_value;
}

fn parseMethod(method_str: []const u8) ?Method {
    if (std.mem.eql(u8, method_str, "GET")) {
        return Method.GET;
    } else if (std.mem.eql(u8, method_str, "POST")) {
        return Method.POST;
    } else if (std.mem.eql(u8, method_str, "PUT")) {
        return Method.PUT;
    } else if (std.mem.eql(u8, method_str, "DELETE")) {
        return Method.DELETE;
    } else {
        return null; // Invalid method
    }
}

fn splitPath(path: []const u8, delim: u8) ![]const []const u8 {
    const allocator = std.heap.page_allocator; // Use an allocator
    var list = std.ArrayList([]const u8).init(allocator);

    // Create a single-element array for the delimiter
    const delim_slice = [1]u8{ delim };
    var it = std.mem.split(u8, path, &delim_slice); // Use & to create a slice

    // Collect segments
    while (it.next()) |segment| {
        try list.append(segment); // If append fails, propagate error
    }

    // Return the dynamic array as a slice
    return list.toOwnedSlice();
}

fn tryParseId(r: zap.Request, id_str: []const u8) ?i32 {
    return std.fmt.parseInt(i32, id_str, 10) catch |err| {
        // Handle invalid ID here
        std.debug.print("Failed to parse ID: {}\n", .{err});
        r.setStatusNumeric(400);
        _ = r.sendBody("{\"error\": \"Invalid ID\"}") catch |err2| {
            std.debug.print("Error sending response: {}\n", .{err2});
        };
        return null; // Return null if the ID couldn't be parsed
    };
}

fn on_request(r: zap.Request) void {
    blk: {
        if (r.method == null) {
            // Handle missing method (e.g., respond with 400 Bad Request)
            r.setStatusNumeric(400);
            _ = r.sendBody("{\"error\": \"Bad Request\"}") catch |err| {
                std.debug.print("Error sending response: {}\n", .{err});
            };
            break :blk;
        }

        const method_str = r.method.?;
        const method_enum = parseMethod(method_str) orelse {
            // Handle invalid HTTP method (e.g., respond with 405 Method Not Allowed)
            r.setStatusNumeric(405);
            _ = r.sendBody("{\"error\": \"Method not allowed\"}") catch |err| {
                std.debug.print("Error sending response: {}\n", .{err});
            };
            break :blk;
        };

        // 1. Set content type to JSON
        r.setContentType(.JSON) catch return;

        // 2. Check the path
        const path = if (r.path) |the_path| the_path else "";

        // TODO: Handle split failure (e.g., allocation error)
        const segments = splitPath(path, '/') catch {
            r.setStatusNumeric(500);
            _ = r.sendBody("{\"error\": \"Internal Server Error\"}") catch |err| {
                std.debug.print("Error sending response: {}\n", .{err});
            };
            break :blk;
        };

        // 3. Compare the method and path segments
        switch (method_enum) {
            Method.GET => {
                if (segments.len == 2 and std.mem.eql(u8, segments[1], "users")) {
                    // GET /users
                    return handleGetAllUsers(r);
                } else if (segments.len == 3 and std.mem.eql(u8, segments[1], "users")) {
                    // GET /users/<id>
                    const id = tryParseId(r, segments[2]) orelse break :blk;
                    return handleGetUserById(r, id);
                } else {
                    // Not found
                    r.setStatusNumeric(404);
                    _ = r.sendBody("{\"error\": \"Not found\"}") catch |err| {
                        std.debug.print("Error sending response: {}\n", .{err});
                    };
                }
            },
            Method.POST => {
                if (segments.len == 2 and std.mem.eql(u8, segments[1], "users")) {
                    // POST /users
                    return handleCreateUser(r);
                } else {
                    r.setStatusNumeric(404);
                    _ = r.sendBody("{\"error\": \"Not found\"}") catch |err| {
                        std.debug.print("Error sending response: {}\n", .{err});
                    };
                }
            },
            Method.PUT => {
                if (segments.len == 3 and std.mem.eql(u8, segments[1], "users")) {
                    // PUT /users/<id>
                    const id = tryParseId(r, segments[2]) orelse break :blk;
                    return handleUpdateUser(r, id);
                } else {
                    r.setStatusNumeric(404);
                    _ = r.sendBody("{\"error\": \"Not found\"}") catch |err| {
                        std.debug.print("Error sending response: {}\n", .{err});
                    };
                }
            },
            Method.DELETE => {
                if (segments.len == 3 and std.mem.eql(u8, segments[1], "users")) {
                    // DELETE /users/<id>
                    const id = tryParseId(r, segments[2]) orelse break :blk;
                    return handleDeleteUser(r, id);
                } else {
                    r.setStatusNumeric(404);
                    _ = r.sendBody("{\"error\": \"Not found\"}") catch |err| {
                        std.debug.print("Error sending response: {}\n", .{err});
                    };
                }
            },
            else => {
                // Method not allowed
                r.setStatusNumeric(405);
                _ = r.sendBody("{\"error\": \"Method not allowed\"}") catch |err| {
                    std.debug.print("Error sending response: {}\n", .{err});
                };
            },
        }
    }
}

fn handleCreateUser(r: zap.Request) void {
    const body = r.body orelse {
        r.setStatusNumeric(400);
        _ = r.sendBody("{\"error\": \"Request body is required\"}") catch {};
        return;
    };

    const result = createUser(body) catch |err| {
        std.debug.print("Error creating user: {}\n", .{err});
        if (err == error.InvalidInput) {
            r.setStatusNumeric(400);
            _ = r.sendBody("{\"error\": \"Invalid input\"}") catch {};
        } else if (err == error.UserNotCreated) {
            r.setStatusNumeric(500);
            _ = r.sendBody("{\"error\": \"Failed to create user\"}") catch {};
        } else if (err == error.Closed) {
            // Handle connection closed error
            r.setStatusNumeric(500);
            r.sendBody("{\"error\": \"Database connection closed\"}") catch |send_err| {
                std.debug.print("Error sending response: {}\n", .{send_err});
            };
        } else {
            r.setStatusNumeric(500);
            _ = r.sendBody("{\"error\": \"Internal server error\"}") catch {};
        }
        return;
    };

    r.sendBody(result) catch |err| {
        std.debug.print("Error sending response: {}\n", .{err});
    };
}

fn handleGetAllUsers(r: zap.Request) void {
    const users_json = queryUsers() catch |err| {
        if (err == error.Closed) {
            // Handle connection closed error
            r.setStatusNumeric(500);
            r.sendBody("{\"error\": \"Database connection closed\"}") catch |send_err| {
                std.debug.print("Error sending response: {}\n", .{send_err});
            };
            return;
        }
        std.debug.print("Error fetching users: {}\n", .{err});
        r.sendBody("{\"error\": \"Failed to fetch users\"}") catch return;
        return;
    };
    r.sendBody(users_json) catch return;
}

fn handleGetUserById(r: zap.Request, id: i32) void {
    const user_json = queryUserById(id) catch |err| {
        std.debug.print("Error fetching user by ID: {}\n", .{err});
        if (err == error.UserNotFound) {
            sendNotFound(r);
        } else if (err == error.Closed) {
            // Handle connection closed error
            r.setStatusNumeric(500);
            r.sendBody("{\"error\": \"Database connection closed\"}") catch |send_err| {
                std.debug.print("Error sending response: {}\n", .{send_err});
            };
        } else {
            r.sendBody("{\"error\": \"Failed to fetch user\"}") catch |send_err| {
                std.debug.print("Error sending response: {}\n", .{send_err});
            };
        }
        return; // Short-circuit on error
    };

    r.sendBody(user_json) catch |err| {
        std.debug.print("Error sending response: {}\n", .{err});
    };
}

fn handleUpdateUser(r: zap.Request, id: i32) void {
    const body = r.body orelse {
        r.setStatusNumeric(400);
        _ = r.sendBody("{\"error\": \"Request body is required\"}") catch {};
        return;
    };

    const result = updateUser(id, body) catch |err| {
        if (err == error.UserNotFound) {
            sendNotFound(r);
        } else if (err == error.Closed) {
            // Handle connection closed error
            r.setStatusNumeric(500);
            r.sendBody("{\"error\": \"Database connection closed\"}") catch |send_err| {
                std.debug.print("Error sending response: {}\n", .{send_err});
            };
        } else {
            std.debug.print("Error updating user: {}\n", .{err});
            r.setStatusNumeric(500);
            _ = r.sendBody("{\"error\": \"Failed to update user\"}") catch {};
        }
        return;
    };

    r.sendBody(result) catch |err| {
        std.debug.print("Error sending response: {}\n", .{err});
    };
}

fn handleDeleteUser(r: zap.Request, id: i32) void {
    const result = deleteUser(id) catch |err| {
        if (err == error.UserNotFound) {
            r.setStatusNumeric(404);
            _ = r.sendBody("{\"error\": \"User not found\"}") catch |send_err| {
                std.debug.print("Error sending response: {}\n", .{send_err});
            };
        } else if (err == error.Closed) {
            // Handle connection closed error
            r.setStatusNumeric(500);
            r.sendBody("{\"error\": \"Database connection closed\"}") catch |send_err| {
                std.debug.print("Error sending response: {}\n", .{send_err});
            };
        } else {
            std.debug.print("Error deleting user: {}\n", .{err});
            r.setStatusNumeric(500);
            _ = r.sendBody("{\"error\": \"Internal server error\"}") catch |send_err| {
                std.debug.print("Error sending response: {}\n", .{send_err});
            };
        }
        return;
    };

    var buf: [20]u8 = undefined;
    const result_str = std.fmt.bufPrint(&buf, "{d}", .{result}) catch |err| {
        std.debug.print("Error formatting result: {}\n", .{err});
        r.setStatusNumeric(500);
        _ = r.sendBody("{\"error\": \"Internal server error\"}") catch |send_err| {
            std.debug.print("Error sending response: {}\n", .{send_err});
        };
        return; // Exit early if formatting fails
    };

    r.sendBody(result_str) catch |err| {
        std.debug.print("Error sending response: {}\n", .{err});
    };
}

fn sendNotFound(r: zap.Request) void {
    r.setStatusNumeric(404);
    r.sendBody("{\"error\": \"Not found\"}") catch return;
}

fn sendBadRequest(r: zap.Request, message: []const u8) void {
    r.setStatusNumeric(400);
    r.sendBody("{\"error\": \"Bad Request: " ++ message ++ "\"}") catch return;
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

    // When `result.next()` returns false, the loop ends
    try json_buf.appendSlice("]");

    return json_buf.toOwnedSlice();
}

fn queryUserById(id: i32) ![]const u8 {
    if (pool == null) {
        return error.PoolNotInitialized;
    }

    var conn = try pool.?.acquire();
    defer conn.release();

    // Allocator to handle temporary memory for query results
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var result = try conn.queryOpts(
        "select * from users where id = $1",
        .{id},
        .{ .allocator = arena.allocator() }
    );
    defer result.deinit();

    // Check for errors or no rows
    const maybe_row = try result.next();
    if (maybe_row == null) {
        return error.UserNotFound; // No rows match the ID
    }

    // Extract the email from the row
    const row = maybe_row.?; // Unwrap the optional
    const email_row_index = 2;
    const email = row.get([]const u8, email_row_index);

    // Build the JSON response
    var json_buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer json_buf.deinit();

    try json_buf.appendSlice("{");
    try json_buf.appendSlice("\"id\":");
    var id_buf: [20]u8 = undefined;
    const id_str = try std.fmt.bufPrint(&id_buf, "{d}", .{id});
    try json_buf.appendSlice(id_str);
    try json_buf.appendSlice(",\"email\":\"");
    try json_buf.appendSlice(email);
    try json_buf.appendSlice("\"}");

    return json_buf.toOwnedSlice(); // Return the JSON string
}

fn createUser(body: []const u8) ![]const u8 {
    if (pool == null) {
        return error.PoolNotInitialized;
    }

    var conn = try pool.?.acquire();
    defer conn.release();

    // Parse the JSON body
    const allocator = std.heap.page_allocator;
    const parsed = try std.json.parseFromSlice(UserInput, allocator, body, .{});
    defer parsed.deinit();

    const user_data = parsed.value;

    const email = user_data.email;

    // Insert into the database
    _ = try conn.execOpts(
        "insert into users (email) values ($1)",
        .{email},
        .{ .allocator = std.heap.page_allocator }
    );

    // Fetch the newly created user (assuming `id` is auto-generated)
    var result = try conn.queryOpts(
        "select id, email from users where email = $1",
        .{email},
        .{ .allocator = std.heap.page_allocator }
    );
    defer result.deinit();

    const maybe_row = try result.next();
    if (maybe_row == null) {
        return error.UserNotCreated;
    }

    const row = maybe_row.?; // Unwrap the row
    const id = row.get(i32, 0); // Column 0 is `id`
    const user_email = row.get([]const u8, 1); // Column 1 is `email`

    // Construct JSON response
    var json_buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer json_buf.deinit();

    try json_buf.appendSlice("{\"id\":");
    var id_buf: [20]u8 = undefined;
    const id_str = try std.fmt.bufPrint(&id_buf, "{d}", .{id});
    try json_buf.appendSlice(id_str);
    try json_buf.appendSlice(",\"email\":\"");
    try json_buf.appendSlice(user_email);
    try json_buf.appendSlice("\"}");

    return json_buf.toOwnedSlice(); // Return JSON response
}

fn updateUser(id: i32, body: []const u8) ![]const u8 {
    if (pool == null) {
        return error.PoolNotInitialized;
    }

    var conn = try pool.?.acquire();
    defer conn.release();

    // Parse the JSON body
    const allocator = std.heap.page_allocator;
    const parsed = try std.json.parseFromSlice(UserInput, allocator, body, .{});
    defer parsed.deinit();

    const user_data = parsed.value;

    // Update the user in the database
    const affected_rows = try conn.execOpts(
        "update users set email = $1 where id = $2",
        .{user_data.email, id},
        .{ .allocator = std.heap.page_allocator }
    );
    if (affected_rows == null or affected_rows == 0) {
        return error.UserNotFound; // No rows were updated
    }

    // Fetch the updated user
    var result = try conn.queryOpts(
        "select id, email from users where id = $1",
        .{id},
        .{ .allocator = std.heap.page_allocator }
    );
    defer result.deinit();

    const maybe_row = try result.next();
    if (maybe_row == null) {
        return error.UserNotFound; // Should not happen if rows were affected
    }

    const row = maybe_row.?; // Unwrap the row
    const email = row.get([]const u8, 1); // Column 1 is `email`

    // Build the JSON response
    var json_buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer json_buf.deinit();

    try json_buf.appendSlice("{\"id\":");
    var id_buf: [20]u8 = undefined;
    const id_str = try std.fmt.bufPrint(&id_buf, "{d}", .{id});
    try json_buf.appendSlice(id_str);
    try json_buf.appendSlice(",\"email\":\"");
    try json_buf.appendSlice(email);
    try json_buf.appendSlice("\"}");

    return json_buf.toOwnedSlice(); // Return JSON response
}

fn deleteUser(id: i32) !i32 {
    if (pool == null) {
        return error.PoolNotInitialized;
    }

    var conn = try pool.?.acquire();
    defer conn.release();

    const affected_rows = try conn.execOpts(
        "delete from users where id = $1",
        .{id},
        .{ .allocator = std.heap.page_allocator }
    );
    if (affected_rows == null or affected_rows == 0) {
        return error.UserNotFound; // No rows were deleted
    }

    const unwrapped_rows: i64 = affected_rows.?;
    return @as(i32, @truncate(unwrapped_rows));
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
