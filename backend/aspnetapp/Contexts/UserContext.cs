using Microsoft.EntityFrameworkCore;
using aspnetapp.Models;

using Npgsql.EntityFrameworkCore.PostgreSQL;
using Microsoft.Extensions.Configuration;


namespace aspnetapp.Contexts
{
    public class UserContext : DbContext
    {
        public UserContext(DbContextOptions<UserContext> options) : base(options) {}

        public DbSet<User>? Users { get; set; }
    }
}
