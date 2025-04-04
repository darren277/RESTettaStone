#!/bin/sh

ldconfig

envsubst '${APACHE_PORT},{$DUMMY_VAR}' < /usr/local/apache2/conf/httpd.conf.template > /usr/local/apache2/conf/httpd.conf

envsubst '${APACHE_PORT},\
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
  ${DUMMY_VAR}'\
  < /usr/local/apache2/conf/locations.conf.template > /usr/local/apache2/conf/locations.conf

envsubst '${REACTAPP_IP},${REACTAPP_PORT},\
  ${VUEAPP_IP},${VUEAPP_PORT},\
  ${GATSBYAPP_IP},${GATSBYAPP_PORT},\
  ${REACTFIBERAPP_IP},${REACTFIBERAPP_PORT},\
  ${NEXTAPP_IP},${NEXTAPP_PORT},\
  ${ANGULARAPP_IP},${ANGULARAPP_PORT}' < /usr/local/apache2/conf/locations-frontend.conf.template > /usr/local/apache2/conf/locations-frontend.conf

echo "Hello, Apache!" > /usr/local/apache2/htdocs/index.html

apachectl -D FOREGROUND
