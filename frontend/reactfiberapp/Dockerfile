FROM node:latest as build-step

ENV NODE_OPTIONS=--openssl-legacy-provider

ARG REACTFIBERAPP_PORT
ENV REACTFIBERAPP_PORT=$REACTFIBERAPP_PORT
ENV PORT=$REACTFIBERAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

WORKDIR /app
ENV PATH /app/node_module/.bin:$PATH
COPY ./package.json /app/
RUN yarn
COPY ./src /app/src
COPY ./public /app/public
RUN touch /app/package-lock.json

EXPOSE $PORT

RUN echo "const ENV_VARS = { \"REACTFIBER_APP_NGINX_HOST\": \"$NGINX_HOST\", \"REACTFIBER_APP_NGINX_PORT\": \"$NGINX_PORT\" }" > /app/src/config.js
RUN echo "export default ENV_VARS" >> /app/src/config.js

RUN npm run build

CMD ["npm" , "start"]
