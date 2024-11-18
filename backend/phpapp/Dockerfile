FROM php:fpm-alpine

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

ARG PHPAPP_PORT
ENV PHPAPP_PORT=$PHPAPP_PORT
ENV PORT=$PHPAPP_PORT

RUN apk add --no-cache postgresql-dev libpq bash
RUN apk add --no-cache php-pgsql

RUN docker-php-ext-install opcache
RUN docker-php-ext-install pdo pdo_pgsql pgsql

COPY ./opcache.ini /usr/local/etc/php/conf.d/opcache.ini
COPY ./ /var/www/html/

EXPOSE $PORT

CMD php -S 0.0.0.0:$PORT -t /var/www/html
