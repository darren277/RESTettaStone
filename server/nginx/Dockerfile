FROM openresty/openresty:alpine-fat

EXPOSE 8084

COPY nginx.conf.template /usr/local/openresty/nginx/conf/nginx.conf.template

COPY sites/*.conf /etc/nginx/sites-enabled/

RUN apk add nano

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
