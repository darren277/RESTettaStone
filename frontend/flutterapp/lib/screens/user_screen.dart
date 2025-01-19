import 'package:flutter/material.dart';
import '../services/api_service.dart';
import '../models/user.dart';

class UserScreen extends StatefulWidget {
    @override
    _UserScreenState createState() => _UserScreenState();
}

class _UserScreenState extends State<UserScreen> {
    final ApiService _apiService = ApiService();
    late Future<List<User>> _users;

    @override
    void initState() {
        super.initState();
        _users = _apiService.getUsers();
    }

    @override
    Widget build(BuildContext context) {
        return Scaffold(
            appBar: AppBar(title: Text('Users')),
            body: FutureBuilder<List<User>>(
                future: _users,
                builder: (context, snapshot) {
                    if (snapshot.connectionState == ConnectionState.waiting) {
                        return Center(child: CircularProgressIndicator());
                    } else if (snapshot.hasError) {
                        return Center(child: Text('Error: ${snapshot.error}'));
                    } else if (!snapshot.hasData || snapshot.data!.isEmpty) {
                        return Center(child: Text('No users found'));
                    } else {
                        final users = snapshot.data!;

                        return ListView.builder(itemCount: users.length, itemBuilder: (context, index) {
                            final user = users[index];

                            return ListTile(
                                title: Text(user.email),
                                subtitle: Text('ID: ${user.id}'),
                                onTap: () async {
                                    final updatedUser = await _apiService.updateUser(user.id, 'new_email@mail.com');
                                    setState(() {_users = _apiService.getUsers();});
                                    ScaffoldMessenger.of(context).showSnackBar(SnackBar(content: Text('Updated user ${updatedUser.email}')));
                                },
                                trailing: IconButton(
                                    icon: Icon(Icons.delete),
                                    onPressed: () async {
                                        await _apiService.deleteUser(user.id);
                                        setState(() {_users = _apiService.getUsers();});
                                    },
                                ),
                            );
                        });
                    }
                },
            ),
            floatingActionButton: FloatingActionButton(
                onPressed: () async {
                    final newUser = await _apiService.createUser('new_user@mail.com');
                    setState(() {_users = _apiService.getUsers();});
                    ScaffoldMessenger.of(context).showSnackBar(SnackBar(content: Text('Created user ${newUser.email}')));
                },
                child: Icon(Icons.add),
            ),
        );
    }
}
