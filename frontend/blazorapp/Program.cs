using BlazorApp.Services;
using BlazorApp.Models;

var builder = WebApplication.CreateBuilder(args);

builder.Services.AddRazorPages();
builder.Services.AddServerSideBlazor();

var NGINX_HOST = Environment.GetEnvironmentVariable("NGINX_HOST");
var NGINX_PORT = Environment.GetEnvironmentVariable("NGINX_PORT");
var FLASK_APP = $"http://{NGINX_HOST}:{NGINX_PORT}/flaskapp";
var USER_API_BASE_URL = $"{FLASK_APP}/users";

builder.Services.AddHttpClient("FlaskApi", client =>
{
    client.BaseAddress = new Uri(FLASK_APP);
});

builder.Services.AddScoped<UserService>();

string port = Environment.GetEnvironmentVariable("PORT") ?? "5000";
builder.WebHost.UseUrls($"http://0.0.0.0:{port}");

var app = builder.Build();

app.MapGet("/users", async (UserService service) =>
    await service.GetAllUsersAsync());

app.MapGet("/users/{id:int}", async (int id, UserService service) =>
    await service.GetUserByIdAsync(id));

app.MapPost("/users", async (User user, UserService service) =>
    await service.CreateUserAsync(user));

app.MapPut("/users/{id:int}", async (int id, User user, UserService service) =>
    await service.UpdateUserAsync(id, user));

app.MapDelete("/users/{id:int}", async (int id, UserService service) =>
    await service.DeleteUserAsync(id));

app.MapBlazorHub();

app.Run();
