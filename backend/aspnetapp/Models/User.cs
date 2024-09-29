using System.ComponentModel.DataAnnotations.Schema;

namespace aspnetapp.Models
{
    [Table("users")]
    public class User {
        [Column("id")]
        public int Id { get; set; }
        [Column("email")]
        public string? Email { get; set; }
    }
}
