import 'dart:convert';
import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as shelf_io;
import 'package:shelf_router/shelf_router.dart';
import 'package:postgres/postgres.dart';

import 'dart:io';

final String _dbHost = Platform.environment['PG_HOST'] as String;
final int _dbPort = int.parse(Platform.environment['PG_PORT'] as String);
final String _dbName = Platform.environment['PG_DB'] as String;
final String _dbUser = Platform.environment['PG_USER'] as String;
final String _dbPassword = Platform.environment['PG_PASS'] as String;

Future<void> main() async {
    final connection = await Connection.open(
        Endpoint(
            host: _dbHost,
            port: _dbPort,
            database: _dbName,
            username: _dbUser,
            password: _dbPassword
        ),
        settings: ConnectionSettings(sslMode: SslMode.disable)
    );

    final router = Router();

    // --- GET /users ---
    router.get('/users', (Request request) async {
        final results = await connection.execute('SELECT id, email FROM users');
        final users = results.map((row) => {'id': row[0], 'email': row[1]}).toList();
        return Response.ok(jsonEncode(users), headers: {'Content-Type': 'application/json'});
    });

    // --- POST /users ---
    router.post('/users', (Request request) async {
        final body = await request.readAsString();
        final data = jsonDecode(body) as Map<String, dynamic>?;

        if (data == null || !data.containsKey('email')) {return Response(400, body: 'Invalid request body: "email" is required.');}

        final email = data['email'];

        // Insert new user
        final result = await connection.execute(Sql.named('INSERT INTO users (email) VALUES (@email) RETURNING id, email'), parameters: {'email': email});

        if (result.isEmpty) {return Response.internalServerError(body: 'Failed to insert user into database.');}

        final insertedUser = {'id': result[0][0], 'email': result[0][1]};

        return Response.ok(jsonEncode(insertedUser), headers: {'Content-Type': 'application/json'});
    });

    // --- GET /users/<id> ---
    router.get('/users/<id|[0-9]+>', (Request request, String id) async {
        final userId = int.tryParse(id);
        if (userId == null) {return Response(400, body: 'Invalid user id.');}

        final result = await connection.execute(Sql.named('SELECT id, email FROM users WHERE id = @id'), parameters: {'id': userId});

        if (result.isEmpty) {return Response(404, body: 'User not found.');}

        final user = {'id': result[0][0], 'email': result[0][1]};

        return Response.ok(jsonEncode(user), headers: {'Content-Type': 'application/json'});
    });

    // --- PUT /users/<id> ---
    router.put('/users/<id|[0-9]+>', (Request request, String id) async {
        final userId = int.tryParse(id);
        if (userId == null) {return Response(400, body: 'Invalid user id.');}

        final body = await request.readAsString();
        final data = jsonDecode(body) as Map<String, dynamic>?;

        if (data == null || !data.containsKey('email')) {return Response(400, body: 'Invalid request body: "email" is required.');}

        final email = data['email'];

        final result = await connection.execute(Sql.named('UPDATE users SET email = @email WHERE id = @id RETURNING id, email'), parameters: {'id': userId, 'email': email});

        if (result.isEmpty) {return Response(404, body: 'User not found or not updated.');}

        final updatedUser = {'id': result[0][0], 'email': result[0][1]};
        return Response.ok(jsonEncode(updatedUser), headers: {'Content-Type': 'application/json'});
    });

    // --- DELETE /users/<id> ---
    router.delete('/users/<id|[0-9]+>', (Request request, String id) async {
        final userId = int.tryParse(id);
        if (userId == null) {return Response(400, body: 'Invalid user id.');}

        final result = await connection.execute(Sql.named('DELETE FROM users WHERE id = @id RETURNING id'), parameters: {'id': userId});

        if (result.isEmpty) {return Response(404, body: 'User not found or not deleted.');}

        return Response.ok('User with id $userId deleted successfully.');
    });

    final handler = Pipeline().addMiddleware(logRequests()).addHandler(router);

    final server = await shelf_io.serve(handler, '0.0.0.0', int.parse(Platform.environment['SHELFAPP_PORT'] ?? '8080'));
    print('Server listening on port ${server.port}');
}
