FROM mcr.microsoft.com/dotnet/sdk:9.0 AS build

WORKDIR /app

ARG BLAZORAPP_PORT
ENV BLAZORAPP_PORT=$BLAZORAPP_PORT
ENV PORT=$BLAZORAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

RUN apt update && apt install -y gettext-base

RUN dotnet new install Bootstrap.Blazor.Templates::9.0.4

RUN dotnet new blazorwasm -n BlazorApp

WORKDIR /app/BlazorApp

COPY ./src .

COPY ./appsettings.json .

COPY ./BlazorApp.csproj .

RUN mv ./appsettings.json ./appsettings.template.json
RUN envsubst '$NGINX_HOST $NGINX_PORT' < ./appsettings.template.json > ./appsettings.json
RUN cp ./appsettings.json ./wwwroot/appsettings.json

COPY ./Program.cs .

RUN dotnet restore "BlazorApp.csproj"
RUN dotnet publish "BlazorApp.csproj" -c Release -o /app/publish


# --- Final Stage with Nginx ---
FROM nginx:alpine AS final

ARG BLAZORAPP_PORT
ENV BLAZORAPP_PORT=$BLAZORAPP_PORT
ENV PORT=$BLAZORAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

# Copy published wasm output into nginx’s html folder
COPY --from=build /app/publish/wwwroot /usr/share/nginx/html

COPY nginx.template.conf /etc/nginx/nginx.template.conf
RUN envsubst '$BLAZORAPP_PORT' < /etc/nginx/nginx.template.conf > /etc/nginx/conf.d/default.conf

EXPOSE $BLAZORAPP_PORT

CMD ["nginx", "-g", "daemon off;"]
