FROM ubuntu:latest

ARG PROLOGAPP_PORT
ENV PORT=$PROLOGAPP_PORT

ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV PG_HOST=$PG_HOST
ENV PG_PORT=$PG_PORT
ENV PG_USER=$PG_USER
ENV PG_PASS=$PG_PASS
ENV PG_DB=$PG_DB

## for apt to be noninteractive
ENV DEBIAN_FRONTEND noninteractive
ENV DEBCONF_NONINTERACTIVE_SEEN true

RUN apt-get update && apt-get install -y swi-prolog

RUN export PATH=/root/.local/bin:/usr/local/bin:/root/.local/bin:$PATH

RUN apt-get update
RUN apt-get install -y unixodbc unixodbc-dev odbc-postgresql
RUN apt-get install -y swi-prolog-odbc

RUN echo "[postgres]" > /etc/odbc.ini
RUN echo "Description = PostgreSQL" >> /etc/odbc.ini
RUN echo "Driver = PostgreSQL" >> /etc/odbc.ini
RUN echo "Servername = $PG_HOST" >> /etc/odbc.ini
RUN echo "Port = $PG_PORT" >> /etc/odbc.ini
RUN echo "Database = $PG_DB" >> /etc/odbc.ini
RUN echo "Username = $PG_USER" >> /etc/odbc.ini
RUN echo "Password = $PG_PASS" >> /etc/odbc.ini

RUN echo "[PostgreSQL]" > /etc/odbcinst.ini
RUN echo "Description = ODBC for PostgreSQL" >> /etc/odbcinst.ini
RUN echo "Driver = /usr/lib/x86_64-linux-gnu/odbc/psqlodbcw.so" >> /etc/odbcinst.ini
RUN echo "Setup = /usr/lib/x86_64-linux-gnu/odbc/libodbcpsqlS.so" >> /etc/odbcinst.ini
RUN echo "FileUsage = 1" >> /etc/odbcinst.ini

COPY . ./

EXPOSE $PORT

ENV GOAL=server($PORT).

ENTRYPOINT swipl -s 'main.pl' -g $GOAL
