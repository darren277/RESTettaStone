class User {
    final int id;
    final String email;

    User({required this.id, required this.email});

    // Factory constructor for creating a User from JSON
    factory User.fromJson(Map<String, dynamic> json) {return User(id: json['id'], email: json['email']);}

    // Method to convert a User to JSON
    Map<String, dynamic> toJson() {return {'id': id, 'email': email};}
}
