using System.Collections.Generic;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Http;
using aspnetapp.Models;
using aspnetapp.Contexts;
using aspnetapp.Repository;
using Microsoft.EntityFrameworkCore;
using System.Linq;

using System.Text.Json;
using System.Text.Json.Serialization;


namespace aspnetapp.Controllers
{
    [Route("users")]
    [ApiController]
    public class UserController : Controller
    {
        private readonly IUserRepository _userRepository;

        public UserController(UserContext context)
        {
            this._userRepository = new UserRepository(context);
        }

        [HttpGet]
        [ProducesResponseType(StatusCodes.Status200OK)]
        [Produces("application/json")]
        public IActionResult Index()
        {
            Response.Clear();
            Response.ContentType = "application/json";
            var users = from user in _userRepository.GetUsers() select user;
            return Ok(users);
        }

        [HttpGet("{id:int}")]
        [ProducesResponseType(StatusCodes.Status200OK)]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        [Produces("application/json")]
        public IActionResult GetUserById(int id)
        {
            Response.Clear();
            Response.ContentType = "application/json";
            var user = _userRepository.GetUserByID(id);
            if (user == null)
            {
                return NotFound(new { error = $"User with id {id} not found." });
            }
            return Ok(user);
        }

        [HttpPost]
        [ProducesResponseType(StatusCodes.Status200OK)]
        [ProducesResponseType(StatusCodes.Status400BadRequest)]
        [Produces("application/json")]
        public IActionResult CreateUser([FromBody] User user)
        {
            Response.Clear();
            Response.ContentType = "application/json";
            if (user == null)
            {
                return BadRequest(new { error = "User object is null." });
            }
            _userRepository.InsertUser(user);
            _userRepository.Save();
            return Ok(user);
        }

        [HttpPut("{id:int}")]
        [ProducesResponseType(StatusCodes.Status200OK)]
        [ProducesResponseType(StatusCodes.Status400BadRequest)]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        [Produces("application/json")]
        public IActionResult UpdateUser(int id, [FromBody] User user)
        {
            Response.Clear();
            Response.ContentType = "application/json";
            if (user == null)
            {
                return BadRequest(new { error = "User object is null." });
            }
            var existingUser = _userRepository.GetUserByID(id);
            if (existingUser == null)
            {
                return NotFound(new { error = $"User with id {id} not found." });
            }
            existingUser.email = user.email;
            _userRepository.UpdateUser(existingUser);
            _userRepository.Save();
            return Ok(existingUser);
        }

        [HttpDelete("{id:int}")]
        [ProducesResponseType(StatusCodes.Status200OK)]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        [Produces("application/json")]
        public IActionResult DeleteUser(int id)
        {
            Response.Clear();
            Response.ContentType = "application/json";
            var user = _userRepository.GetUserByID(id);
            if (user == null)
            {
                return NotFound(new { error = $"User with id {id} not found." });
            }
            _userRepository.DeleteUser(id);
            _userRepository.Save();
            return Ok(user);
        }
    }
}
