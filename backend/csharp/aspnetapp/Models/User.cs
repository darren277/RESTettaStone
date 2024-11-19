using System.ComponentModel.DataAnnotations.Schema;

namespace aspnetapp.Models
{
    [Table("users")]
    public class User {
        [Column("id")]
        public int id { get; set; }
        [Column("email")]
        public string? email { get; set; }
    }
}
