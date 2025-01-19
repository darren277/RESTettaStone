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

    void _showCreateUserDialog() {
        final TextEditingController emailController = TextEditingController();

        showDialog(
            context: context,
            builder: (BuildContext context) {
                return AlertDialog(
                    title: Text('Create User'),
                    content: TextField(controller: emailController, decoration: InputDecoration(labelText: 'Email')),
                    actions: [
                        TextButton(onPressed: () => Navigator.of(context).pop(), child: Text('CANCEL')),
                        TextButton(
                            onPressed: () async {
                                final newUser = await _apiService.createUser(emailController.text);
                                setState(() {_users = _apiService.getUsers();});
                                Navigator.of(context).pop();
                                ScaffoldMessenger.of(context).showSnackBar(SnackBar(content: Text('Created user ${newUser.email}')));
                            },
                            child: Text('SAVE'),
                        ),
                    ],
                );
            },
        );
    }

    void _showUpdateDialog(BuildContext context, User user) {
        final TextEditingController emailController = TextEditingController(text: user.email);

        showDialog(
            context: context,
            builder: (BuildContext context) {
                return AlertDialog(
                    title: Text('Update Email'),
                    content: TextField(controller: emailController, decoration: InputDecoration(labelText: 'New Email')),
                    actions: [
                        TextButton(onPressed: () => Navigator.of(context).pop(), child: Text('CANCEL')),
                        TextButton(
                            onPressed: () async {
                                final updatedUser = await _apiService.updateUser(user.id, emailController.text);
                                setState(() {_users = _apiService.getUsers();});
                                Navigator.of(context).pop();
                                ScaffoldMessenger.of(context).showSnackBar(SnackBar(content: Text('Updated user to ${updatedUser.email}')));
                            },
                            child: Text('SAVE'),
                        ),
                    ],
                );
            },
        );
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
                                    _showUpdateDialog(context, user);
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
                onPressed: () => _showCreateUserDialog(),
                child: Icon(Icons.add),
            ),
        );
    }
}
