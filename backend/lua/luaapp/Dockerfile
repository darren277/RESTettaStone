FROM openresty/openresty:alpine-fat

ARG LUAAPP_PORT
ENV PORT=$LUAAPP_PORT

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

RUN luarocks install pgmoon
RUN luarocks install lua-cjson
RUN luarocks install lua-resty-openssl

COPY nginx.conf /usr/local/openresty/nginx/conf/nginx.conf.template

EXPOSE $PORT

RUN envsubst '${PORT}' < /usr/local/openresty/nginx/conf/nginx.conf.template > /usr/local/openresty/nginx/conf/nginx.conf

ENTRYPOINT /usr/local/openresty/bin/openresty -g 'daemon off;'
