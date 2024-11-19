FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build-env

WORKDIR /app

COPY *.csproj ./
RUN dotnet restore

COPY . ./
RUN dotnet publish -c Release -o out


FROM mcr.microsoft.com/dotnet/aspnet:6.0

ARG ASPNETAPP_PORT
ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV ASPNETAPP_PORT=$ASPNETAPP_PORT
ENV PG_HOST=$PG_HOST
ENV PG_PORT=$PG_PORT
ENV PG_USER=$PG_USER
ENV PG_PASS=$PG_PASS
ENV PG_DB=$PG_DB

ENV URL="http://0.0.0.0:${ASPNETAPP_PORT}"
ENV PG_HOST_SED_STRING="s/YOURSERVER/${PG_HOST}/g"
ENV PG_PORT_SED_STRING="s/YOURPORT/${PG_PORT}/g"
ENV PG_USER_SED_STRING="s/YOURUSERID/${PG_USER}/g"
ENV PG_PASS_SED_STRING="s/YOURPASSWORD/${PG_PASS}/g"
ENV PG_DB_SED_STRING="s/YOURDATABASE/${PG_DB}/g"

WORKDIR /app
COPY --from=build-env /app/out .

RUN sed -i $PG_HOST_SED_STRING appsettings.json
RUN sed -i $PG_PORT_SED_STRING appsettings.json
RUN sed -i $PG_USER_SED_STRING appsettings.json
RUN sed -i $PG_PASS_SED_STRING appsettings.json
RUN sed -i $PG_DB_SED_STRING appsettings.json

EXPOSE $ASPNETAPP_PORT
ENTRYPOINT dotnet aspnetapp.dll --urls $URL
