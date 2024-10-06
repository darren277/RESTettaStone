FROM openresty/openresty:alpine-fat

ARG ENTRYPOINT_VERSION=
#ARG ENTRYPOINT_VERSION=2

EXPOSE 8084

COPY nginx.conf.template /usr/local/openresty/nginx/conf/nginx.conf.template

COPY upstreams*.conf.template /usr/local/openresty/nginx/conf/

#COPY locations*.conf /usr/local/openresty/nginx/conf/
COPY locations.conf /usr/local/openresty/nginx/conf/

RUN ls
COPY entrypoint$ENTRYPOINT_VERSION.sh /entrypoint.sh

#COPY sites/*.conf /etc/nginx/sites-enabled/

RUN apk add nano

RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
