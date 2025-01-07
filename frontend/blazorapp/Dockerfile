FROM mcr.microsoft.com/dotnet/sdk:9.0 AS build

WORKDIR /app

ARG BLAZORAPP_PORT
ENV BLAZORAPP_PORT=$BLAZORAPP_PORT
ENV PORT=$BLAZORAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

RUN dotnet new install Bootstrap.Blazor.Templates::9.0.4

RUN dotnet new blazorwasm -n BlazorApp

WORKDIR /app/BlazorApp

COPY ./src .

COPY ./appsettings.json .
COPY ./Program.cs .

RUN dotnet restore "BlazorApp.csproj"
RUN dotnet publish "BlazorApp.csproj" -c Release -o /app/publish


FROM mcr.microsoft.com/dotnet/aspnet:9.0 AS final

ARG BLAZORAPP_PORT
ENV BLAZORAPP_PORT=$BLAZORAPP_PORT
ENV PORT=$BLAZORAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

WORKDIR /app

COPY --from=build /app/publish .

EXPOSE $BLAZORAPP_PORT

#ENTRYPOINT ["dotnet", "BlazorApp.dll"]
ENTRYPOINT tail -f /dev/null
