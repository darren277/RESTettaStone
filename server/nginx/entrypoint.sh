#!/bin/sh

ldconfig

envsubst '${NGINX_PORT},{$DUMMY_VAR}' < /usr/local/openresty/nginx/conf/nginx.conf.template > /usr/local/openresty/nginx/conf/nginx.conf

envsubst '${NGINX_PORT},\
  ${CROWAPP_IP},${CROWAPP_PORT},\
  ${ASPNETAPP_IP},${ASPNETAPP_PORT},\
  ${SPRINGBOOTAPP_IP},${SPRINGBOOTAPP_PORT},\
  ${RAILSAPP_IP},${RAILSAPP_PORT},\
  ${PERLAPP_IP},${PERLAPP_PORT},\
  ${ACTIXAPP_IP},${ACTIXAPP_PORT},\
  ${SWIFTAPP_IP},${SWIFTAPP_PORT},\
  ${GOAPP_IP},${GOAPP_PORT},\
  ${BUNAPP_IP},${BUNAPP_PORT},\
  ${ZIGAPP_IP},${ZIGAPP_PORT},\
  ${DUMMY_VAR}'\
  < /usr/local/openresty/nginx/conf/upstreams.conf.template > /usr/local/openresty/nginx/conf/upstreams.conf

/usr/local/openresty/nginx/sbin/nginx -g 'daemon off;'
while true; do sleep 1d; done
