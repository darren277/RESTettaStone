#!/bin/sh
set -e

rm -f /etc/nginx/conf.d/default.conf

envsubst '${SVELTEAPP_PORT}' < /etc/nginx/conf.d/default.template.conf > /etc/nginx/conf.d/default.conf

rm -f /etc/nginx/conf.d/default.template.conf

exec nginx -g "daemon off;"
