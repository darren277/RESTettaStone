package springbootapp.springbootapp.controller;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.http.ResponseEntity;
import springbootapp.springbootapp.exception.ResourceNotFoundException;

import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

import springbootapp.springbootapp.model.User;
import springbootapp.springbootapp.repository.UserRepository;


@RestController
@RequestMapping("/")
public class UserController {

    @Autowired
    private UserRepository userRepository;

    @GetMapping("/users")
    public List<User> getAllUsers() {
        return this.userRepository.findAll();
    }

    @GetMapping("/users/{id}")
    public User getUserById(@PathVariable Long id) {
        return this.userRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("User not found with id :" + id));
    }

    @PostMapping("/users")
    public User createUser(@RequestBody User user) {
        return this.userRepository.save(user);
    }

    @PutMapping("/users/{id}")
    public User updateUser(@RequestBody User user, @PathVariable long id) {
        User existingUser = this.userRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("User not found with id :" + id));
        existingUser.setEmail(user.getEmail());
        return this.userRepository.save(existingUser);
    }

    @DeleteMapping("/users/{id}")
    public ResponseEntity<User> deleteUser(@PathVariable long id) {
        User existingUser = this.userRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("User not found with id :" + id));
        this.userRepository.delete(existingUser);
        return ResponseEntity.ok().build();
    }
}
