package springbootapp.springbootapp.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import springbootapp.springbootapp.model.User;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {

}
