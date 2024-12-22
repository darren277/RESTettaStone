FROM php:latest

ARG FATFREEAPP_PORT
ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV PORT $FATFREEAPP_PORT
ENV PG_HOST $PG_HOST
ENV PG_PORT $PG_PORT
ENV PG_USER $PG_USER
ENV PG_PASS $PG_PASS
ENV PG_DB $PG_DB

ENV COMPOSER_COMMIT_HASH 11cb825ad3d659a4f63fe591226b8f3545897914
ENV INSTALL_SH="wget https://raw.githubusercontent.com/composer/getcomposer.org/${COMPOSER_COMMIT_HASH}/web/installer -O - | php"

WORKDIR /var/www/html

RUN apt-get update && apt-get install -y git unzip wget

RUN apt-get install -y libpq-dev
RUN docker-php-ext-install pdo pdo_pgsql

COPY ./composer.json /var/www/html
COPY ./.htaccess /var/www/html

RUN echo $INSTALL_SH > /var/www/html/install_composer.sh
RUN chmod +x /var/www/html/install_composer.sh
RUN /var/www/html/install_composer.sh

RUN git clone https://github.com/f3-factory/fatfree-core.git vendor/fatfree-core

RUN php composer.phar install

COPY ./src /var/www/html

EXPOSE $PORT

CMD php -S 0.0.0.0:$PORT -t /var/www/html/
