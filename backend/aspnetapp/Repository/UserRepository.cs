using System;
using System.Collections.Generic;
using System.Linq;
using aspnetapp.Models;
using aspnetapp.Contexts;
using System.Data;
using Microsoft.EntityFrameworkCore;


namespace aspnetapp.Repository
{
    public class UserRepository : IUserRepository
    {
        private UserContext _context;
        public UserRepository(UserContext userContext) {this._context = userContext;}
        public IEnumerable<User> GetUsers() {return _context.Users.ToList();}
        public User GetUserByID(int id) {return _context.Users.Find(id);}
        public void InsertUser(User user) {_context.Users.Add(user);}
        public void DeleteUser(int userID) {User user = _context.Users.Find(userID); _context.Users.Remove(user);}
        public void UpdateUser(User user) {_context.Entry(user).State = EntityState.Modified;}
        public void Save() {_context.SaveChanges();}
        private bool disposed = false;
        protected virtual void Dispose(bool disposing) {if (!this.disposed) {if (disposing) {_context.Dispose();}} this.disposed = true;}
        public void Dispose() {Dispose(true); GC.SuppressFinalize(this);}
    }
}
