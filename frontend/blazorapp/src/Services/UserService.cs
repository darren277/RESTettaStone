using System.Net.Http;
using System.Net.Http.Json;
using BlazorApp.Models;

namespace BlazorApp.Services
{
    public class UserService
    {
        private readonly HttpClient _http;

        public UserService(HttpClient http)
        {
            _http = http;
        }

        public async Task<List<User>?> GetAllUsersAsync()
        {
            return await _http.GetFromJsonAsync<List<User>>("users");
        }

        public async Task<User?> GetUserByIdAsync(int id)
        {
            return await _http.GetFromJsonAsync<User>($"users/{id}");
        }

        public async Task<User?> CreateUserAsync(User user)
        {
            var response = await _http.PostAsJsonAsync("users", user);
            if (!response.IsSuccessStatusCode) return null;
            return await response.Content.ReadFromJsonAsync<User>();
        }

        public async Task<bool> UpdateUserAsync(int id, User user)
        {
            var response = await _http.PutAsJsonAsync($"users/{id}", user);
            return response.IsSuccessStatusCode;
        }

        public async Task<bool> DeleteUserAsync(int id)
        {
            var response = await _http.DeleteAsync($"users/{id}");
            return response.IsSuccessStatusCode;
        }
    }
}
