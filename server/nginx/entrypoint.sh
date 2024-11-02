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
  ${VIBEAPP_IP},${VIBEAPP_PORT},\
  ${FSHARPAPP_IP},${FSHARPAPP_PORT},\
  ${PROLOGAPP_IP},${PROLOGAPP_PORT},\
  ${SYMFONYAPP_IP},${SYMFONYAPP_PORT},\
  ${ROCKETAPP_IP},${ROCKETAPP_PORT},\
  ${NODEAPP_IP},${NODEAPP_PORT},\
  ${LUAAPP_IP},${LUAAPP_PORT},\
  ${PLAYAPP_IP},${PLAYAPP_PORT},\
  ${FLASKAPP_IP},${FLASKAPP_PORT},\
  ${LARAVELAPP_IP},${LARAVELAPP_PORT},\
  ${DJANGOAPP_IP},${DJANGOAPP_PORT},\
  ${DUMMY_VAR}'\
  < /usr/local/openresty/nginx/conf/upstreams.conf.template > /usr/local/openresty/nginx/conf/upstreams.conf

envsubst '${REACTAPP_IP},${REACTAPP_PORT},${VUEAPP_IP},${VUEAPP_PORT},${ANGULARAPP_IP},${ANGULARAPP_PORT},${GATSBYAPP_IP},${GATSBYAPP_PORT}' < /usr/local/openresty/nginx/conf/upstreams-frontend.conf.template > /usr/local/openresty/nginx/conf/upstreams-frontend.conf

/usr/local/openresty/nginx/sbin/nginx -g 'daemon off;'
while true; do sleep 1d; done
