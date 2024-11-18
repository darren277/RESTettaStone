using System.Collections.Generic;
using Microsoft.AspNetCore.Mvc;
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
    public class UserController : Controller
    {
        private readonly IUserRepository _userRepository;

        public UserController(UserContext context)
        {
            this._userRepository = new UserRepository(context);
        }

        [ProducesResponseType(StatusCodes.Status200OK)]
        [Produces("application/json")]
        public IActionResult Index()
        {
            // var users = _userRepository.GetUsers();
            Response.Clear();
            Response.ContentType = "application/json";
            var users = from user in _userRepository.GetUsers() select user;
            return Json(users);
        }
    }
}
