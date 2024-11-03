FROM node:20-alpine

ARG GATSBYAPP_PORT
ENV GATSBYAPP_PORT=$GATSBYAPP_PORT
ENV PORT=$GATSBYAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

WORKDIR /app

RUN apk add --no-cache --virtual .gyp python3 make g++
RUN apk add --no-cache git util-linux openssl autoconf automake libtool gcc file

EXPOSE $PORT

RUN yarn global add gatsby-cli && yarn cache clean

COPY ./package.json /app/package.json

RUN yarn

COPY ./gatsby-config.js /app/gatsby-config.js
COPY ./gatsby-node.js /app/gatsby-node.js

COPY ./src /app/src

RUN echo "GATSBY_APP_NGINX_HOST=$NGINX_HOST" > /app/.env
RUN echo "GATSBY_APP_NGINX_PORT=$NGINX_PORT" >> /app/.env

RUN apk add --no-cache bash

#CMD ["tail", "-f", "/dev/null"]
# Using the same `bash -c` + `sleep` trick as in the Symfony container.
# See README.md/Gotchas, Symfony section, for details.
ENTRYPOINT bash -c "sleep 5 && gatsby build --prefix-paths && gatsby serve --host 0.0.0.0 --port $PORT --prefix-paths"

# DO NOT FORGET THE `/` AT THE END OF THE URL
# e.g. http://127.0.0.1:8084/gatsbyapp/users/
