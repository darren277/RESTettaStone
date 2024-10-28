FROM php:7.3-fpm

# Arguments defined in docker-compose.yml
ARG user=laravel
ARG uid=1000

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git \
    curl \
    libpng-dev \
    libonig-dev \
    libxml2-dev \
    zip \
    unzip \
    libzip-dev \
    -y mariadb-client

# Clear cache
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

# Install PHP extensions
RUN docker-php-ext-install zip mysqli pdo_mysql mbstring exif pcntl bcmath gd && docker-php-ext-enable mysqli

# postgres:
RUN apt-get update && apt-get install -y libpq-dev
RUN docker-php-ext-install pdo_pgsql pgsql


COPY --from=composer:latest /usr/bin/composer /usr/bin/composer

# Create system user to run Composer and Artisan Commands
RUN useradd -G www-data,root -u $uid -d /home/$user $user
RUN mkdir -p /home/$user/.composer && \
    chown -R $user:$user /home/$user

WORKDIR /var/www

USER $user

COPY --chown=$user:$user ./app /var/www/app
COPY --chown=$user:$user ./bootstrap /var/www/bootstrap
COPY --chown=$user:$user ./config /var/www/config
COPY --chown=$user:$user ./public /var/www/public
COPY --chown=$user:$user ./routes /var/www/routes
COPY --chown=$user:$user ./artisan /var/www/artisan
COPY --chown=$user:$user ./composer.json /var/www/composer.json
COPY --chown=$user:$user ./composer.sh /var/www/composer.sh
COPY --chown=$user:$user ./package.json /var/www/package.json
COPY --chown=$user:$user ./pg.php /var/www/pg.php
COPY --chown=$user:$user ./server.php /var/www/server.php
COPY --chown=$user:$user ./webpack.mix.js /var/www/webpack.mix.js

RUN chown -R $user:$user ~/.composer/

USER root
RUN chmod -R 775 /var/www/
USER $user

RUN mkdir -p bootstrap/cache
RUN mkdir -p storage
RUN mkdir -p storage/framework
RUN mkdir -p storage/framework/cache
RUN mkdir -p storage/framework/sessions
RUN mkdir -p storage/framework/views

USER root
RUN chmod -R 775 bootstrap/cache
RUN chmod -R 775 storage

USER $user
USER root

RUN composer install

RUN chmod -R 775 /var/www/

USER $user

# storage/logs/laravel.log

ARG LARAVELAPP_PORT
ENV LARAVELAPP_PORT=$LARAVELAPP_PORT
ENV PORT=$LARAVELAPP_PORT

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

ARG SECRET_KEY
ENV SECRET_KEY=$SECRET_KEY

USER root

COPY ./env_vars ./.env
RUN chmod 775 .env

USER $user

RUN echo "\n\nDB_CONNECTION=pgsql" >> .env
RUN echo "DB_HOST=$PG_HOST" >> .env
RUN echo "DB_PORT=$PG_PORT" >> .env
RUN echo "DB_DATABASE=$PG_DB" >> .env
RUN echo "DB_USERNAME=$PG_USER" >> .env
RUN echo "DB_PASSWORD=$PG_PASS" >> .env

RUN echo "APP_KEY=$SECRET_KEY\n\n" >> .env

EXPOSE $PORT

ENTRYPOINT php artisan serve --host=0.0.0.0 --port=$PORT
