using Microsoft.AspNetCore.Components.Web;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using BlazorApp;
using BlazorApp.Services;
using BlazorApp.Models;

var builder = WebAssemblyHostBuilder.CreateDefault(args);

// If you have a component called <App /> at the root (App.razor):
builder.RootComponents.Add<App>("#app");

var NGINX_HOST = builder.Configuration["NGINX_HOST"];
var NGINX_PORT = builder.Configuration["NGINX_PORT"];
var FLASK_APP = $"http://{NGINX_HOST}:{NGINX_PORT}/flaskapp";
var USER_API_BASE_URL = $"{FLASK_APP}/users";

Console.WriteLine($"NGINX_HOST={NGINX_HOST}");
Console.WriteLine($"NGINX_PORT={NGINX_PORT}");
Console.WriteLine($"FLASK_APP={FLASK_APP}");
Console.WriteLine($"USER_API_BASE_URL={USER_API_BASE_URL}");

builder.Services.AddScoped(sp => new HttpClient
{
    BaseAddress = new Uri(USER_API_BASE_URL)
});

builder.Services.AddScoped<UserService>();

string port = Environment.GetEnvironmentVariable("PORT") ?? "5000";

await builder.Build().RunAsync();
