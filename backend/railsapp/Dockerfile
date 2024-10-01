FROM ruby:3.3

ARG RAILSAPP_PORT
ARG PG_HOST
ARG PG_USER
ARG PG_PASS
ARG PG_DB
ARG PG_PORT

RUN apt-get update -qq && apt-get install -y nodejs postgresql-client
WORKDIR /usr/src/
RUN ls
COPY Gemfile ./Gemfile
COPY Gemfile.lock ./Gemfile.lock
COPY Rakefile ./Rakefile
COPY config.ru ./config.ru
RUN bundle install

ENV PG_HOST=$PG_HOST
ENV PG_USER=$PG_USER
ENV PG_PASS=$PG_PASS
ENV PG_DB=$PG_DB
ENV PG_PORT=$PG_PORT
ENV PORT=$RAILSAPP_PORT

COPY ./app ./app
COPY ./config ./config
COPY ./bin ./bin

COPY entrypoint.sh ./entrypoint.sh
RUN chmod +x /usr/src/entrypoint.sh

EXPOSE $PORT

ENTRYPOINT ["/usr/src/entrypoint.sh"]

CMD rails server -b 0.0.0.0 -p $PORT
