FROM httpd:latest

RUN apt update && apt install -y gettext

RUN mkdir -p /usr/local/apache2/htdocs/public-html
RUN echo "<h1>Hello, Apache!</h1>" > /usr/local/apache2/htdocs/public-html/index.html

COPY ./httpd.conf.template /usr/local/apache2/conf/httpd.conf.template
COPY ./locations.conf /usr/local/apache2/conf/locations.conf.template
COPY ./locations-frontend.conf /usr/local/apache2/conf/locations-frontend.conf.template

COPY ./entrypoint.sh /usr/local/apache2/entrypoint.sh
RUN chmod +x /usr/local/apache2/entrypoint.sh

ENTRYPOINT ./entrypoint.sh
# cat /usr/local/apache2/logs/error_log
