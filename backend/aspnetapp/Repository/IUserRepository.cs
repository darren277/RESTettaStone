using System;
using System.Collections.Generic;
using aspnetapp.Models;

namespace aspnetapp.Repository
{
    public interface IUserRepository : IDisposable
    {
        IEnumerable<User> GetUsers();
        User GetUserByID(int userId);
        void InsertUser(User users);
        void DeleteUser(int userID);
        void UpdateUser(User user);
        void Save();
    }
}
