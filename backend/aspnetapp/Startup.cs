using System;
using System.IO;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.FileProviders;
using aspnetapp.Contexts;
using aspnetapp.Controllers;
using aspnetapp.Models;


namespace AspNetApp
{
    public class Startup
    {
        public Startup(IConfiguration configuration) {Configuration = configuration;}

        public IConfiguration Configuration { get; set; }

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            //Add PostgreSQL support
            services.AddDbContext<UserContext>(options => options.UseNpgsql(Configuration["Data:DbContext:ConnectionString"]));

            services.AddControllersWithViews();

            services.AddControllers().AddXmlSerializerFormatters();
            services.AddControllers().AddJsonOptions(options => options.JsonSerializerOptions.PropertyNamingPolicy = null);

            services.AddCors(o => o.AddPolicy("AllowAllPolicy", options => {options.AllowAnyOrigin().AllowAnyMethod().AllowAnyHeader();}));

            services.AddRouting(options => options.LowercaseUrls = true);
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment()) {app.UseDeveloperExceptionPage();} else {app.UseExceptionHandler("/Home/Error");}

            app.UseCors("AllowAllPolicy");

            app.UseRouting();

            app.UseEndpoints(endpoints => {endpoints.MapControllerRoute(name: "default", pattern: "{controller=Home}/{action=Index}/{id?}");});
        }
    }
}
