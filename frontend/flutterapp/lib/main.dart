import 'package:flutter/material.dart';
import 'screens/user_screen.dart';

void main() {
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter REST API Demo',
      theme: ThemeData(primarySwatch: Colors.blue),
      home: UserScreen(),
    );
  }
}
