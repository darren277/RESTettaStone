package springbootapp.springbootapp.controller;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.GetMapping;

import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

import springbootapp.springbootapp.model.User;
import springbootapp.springbootapp.repository.UserRepository;


@RestController
@RequestMapping("/api/v1")
public class UserController {

    @Autowired
    private UserRepository userRepository;

    @GetMapping("/users")
    public List<User> getAllUsers() {
        return this.userRepository.findAll();
    }

    // Get user by id
    // public User getUserById(long id) {return this.userRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("User not found with id :" + id));}
}
