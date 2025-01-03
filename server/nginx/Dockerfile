FROM openresty/openresty:alpine-fat

ARG NGINX_PORT
ARG ENTRYPOINT_VERSION=

ARG LOCATIONS=

EXPOSE $NGINX_PORT

COPY nginx.conf.template /usr/local/openresty/nginx/conf/nginx.conf.template

COPY upstreams*.conf.template /usr/local/openresty/nginx/conf/

COPY locations.conf /usr/local/openresty/nginx/conf/
COPY locations-frontend.conf /usr/local/openresty/nginx/conf/
COPY locations$LOCATIONS.conf /usr/local/openresty/nginx/conf/

COPY entrypoint$ENTRYPOINT_VERSION.sh /entrypoint.sh

# nginx: [emerg] open() "/var/log/nginx/access.log" failed (2: No such file or directory)
RUN mkdir -p /var/log/nginx

# For debugging inside the container:
RUN apk add nano

RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
