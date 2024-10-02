#!/bin/sh

ldconfig

envsubst '${NGINX_PORT},${CROWAPP_IP},${CROWAPP_PORT},${ASPNETAPP_IP},${ASPNETAPP_PORT},\
  ${SPRINGBOOTAPP_IP},${SPRINGBOOTAPP_PORT},${RAILSAPP_IP},${RAILSAPP_PORT},${PERLAPP_IP},${PERLAPP_PORT}'\
  < /usr/local/openresty/nginx/conf/nginx.conf.template > /usr/local/openresty/nginx/conf/nginx.conf

/usr/local/openresty/nginx/sbin/nginx -g 'daemon off;'
while true; do sleep 1d; done
