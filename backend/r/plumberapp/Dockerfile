FROM rstudio/plumber:latest

ARG PLUMBERAPP_PORT
ENV PLUMBERAPP_PORT=$PLUMBERAPP_PORT
ENV PORT=$PLUMBERAPP_PORT

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

# Install PostgreSQL client libraries
RUN apt-get update && apt-get install -y libpq-dev && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('RPostgres')"
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('r-dbi/RPostgres')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('jsonlite')"
RUN R -e "install.packages('httr')"
RUN R -e "install.packages('DBI')"
RUN R -e "install.packages('plumber')"

COPY ./plumberapp.r /app/plumberapp.R
COPY ./server.r /app/server.R

EXPOSE $PLUMBERAPP_PORT

RUN tr -d '\r' < ./app/plumberapp.R > ./app/plumberapp.tmp && mv ./app/plumberapp.tmp ./app/plumberapp.R
RUN chmod +x ./app/plumberapp.R

RUN tr -d '\r' < ./app/server.R > ./app/server.tmp && mv ./app/server.tmp ./app/server.R
RUN chmod +x ./app/server.R

#ENTRYPOINT bash
ENTRYPOINT Rscript ./app/server.R
