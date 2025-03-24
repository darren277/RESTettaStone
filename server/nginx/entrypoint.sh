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
  ${FIREBASEAPP_IP},${FIREBASEAPP_PORT},\
  ${TOMCATAPP_IP},${TOMCATAPP_PORT},\
  ${FATFREEAPP_IP},${FATFREEAPP_PORT},\
  ${PHPAPP_IP},${PHPAPP_PORT},\
  ${SPOCKAPP_IP},${SPOCKAPP_PORT},\
  ${HUNCHENTOOTAPP_IP},${HUNCHENTOOTAPP_PORT},\
  ${PASCALAPP_IP},${PASCALAPP_PORT},\
  ${BASHAPP_IP},${BASHAPP_PORT},\
  ${PHOENIXAPP_IP},${PHOENIXAPP_PORT},\
  ${HTTP4KAPP_IP},${HTTP4KAPP_PORT},\
  ${SHELFAPP_IP},${SHELFAPP_PORT},\
  ${PLUMBERAPP_IP},${PLUMBERAPP_PORT},\
  ${KITAPP_IP},${KITAPP_PORT},\
  ${GENIEAPP_IP},${GENIEAPP_PORT},\
  ${DUMMY_VAR}'\
  < /usr/local/openresty/nginx/conf/upstreams.conf.template > /usr/local/openresty/nginx/conf/upstreams.conf

envsubst '${REACTAPP_IP},${REACTAPP_PORT},\
  ${VUEAPP_IP},${VUEAPP_PORT},\
  ${GATSBYAPP_IP},${GATSBYAPP_PORT},\
  ${REACTFIBERAPP_IP},${REACTFIBERAPP_PORT},\
  ${NEXTAPP_IP},${NEXTAPP_PORT},\
  ${BLAZORAPP_IP},${BLAZORAPP_PORT},\
  ${SVELTEAPP_IP},${SVELTEAPP_PORT},\
  ${PYTHONREACTAPP_IP},${PYTHONREACTAPP_PORT},\
  ${ANGULARAPP_IP},${ANGULARAPP_PORT}' < /usr/local/openresty/nginx/conf/upstreams-frontend.conf.template > /usr/local/openresty/nginx/conf/upstreams-frontend.conf

/usr/local/openresty/nginx/sbin/nginx -g 'daemon off;'
while true; do sleep 1d; done
