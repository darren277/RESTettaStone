FROM php:8.2-fpm

ARG SYMFONYAPP_PORT
ENV SYMFONYAPP_PORT=$SYMFONYAPP_PORT

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

WORKDIR /var/www

# Add user with uid:gid of 1000:1000 to prevent right access in linux os
ENV USER=docker
ENV UID=1000
ENV GID=1000

RUN apt-get update && apt-get install -y git

RUN addgroup --gid "${GID}" "${USER}"
RUN adduser --disabled-password --gecos "" --home "/var/www" --ingroup "$USER" --no-create-home --uid "$UID" "$USER"

RUN ln -s /usr/bin/php7 /usr/bin/php

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

RUN curl -sS https://get.symfony.com/cli/installer | bash
RUN mv /root/.symfony5/bin/symfony /usr/local/bin/symfony


RUN git config --global user.email "you@example.com"
RUN git config --global user.name "Your Name"

RUN symfony new --webapp symfonyapp

RUN cd symfonyapp && composer install

COPY ./src ./symfonyapp/src

ENV PORT=$SYMFONYAPP_PORT

EXPOSE $PORT

WORKDIR /var/www/symfonyapp

RUN apt-get update && apt-get install -y libpq-dev
RUN docker-php-ext-install pdo pdo_mysql pdo_pgsql
RUN composer require symfony/orm-pack

ENV DATABASE_URL="postgresql://${PG_USER}:${PG_PASS}@${PG_HOST}:${PG_PORT}/${PG_DB}"

# NOTE: Due to a very bizarre constraint with the Symfony CLI, this 5 second delay is required to keep it from exiting immediately.
#ENTRYPOINT symfony serve --port=$PORT
ENTRYPOINT bash -c "sleep 5 && symfony serve --port=${PORT} --listen-ip=0.0.0.0"
