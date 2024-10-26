FROM node:16-alpine as build-step

ARG REACTAPP_PORT
ENV REACTAPP_PORT=$REACTAPP_PORT
ENV PORT=$REACTAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

WORKDIR /app
ENV PATH /app/node_module/.bin:$PATH
COPY ./package.json /app/
RUN touch /app/package-lock.json
RUN npm install
COPY ./src /app/src
COPY ./public /app/public
EXPOSE $REACTAPP_PORT

RUN echo "REACT_APP_NGINX_HOST=$NGINX_HOST" > /app/.env
RUN echo "REACT_APP_NGINX_PORT=$NGINX_PORT" >> /app/.env

CMD ["npm" , "start"]
