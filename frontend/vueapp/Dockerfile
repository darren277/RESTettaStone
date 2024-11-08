FROM node:14.17.0-alpine

ARG VUEAPP_PORT
ENV VUEAPP_PORT=$VUEAPP_PORT
ENV PORT=$VUEAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

RUN npm install -g http-server
RUN apk add g++ make py3-pip

WORKDIR /app

COPY package*.json ./

RUN npm install

COPY ./.babelrc ./
COPY ./webpack.config.js ./
COPY ./public ./public

COPY ./src ./src

RUN echo "VUE_APP_NGINX_HOST=$NGINX_HOST" > /app/.env
RUN echo "VUE_APP_NGINX_PORT=$NGINX_PORT" >> /app/.env

RUN npm run build

RUN cp ./public/index.html ./index.html

EXPOSE $PORT

CMD ["npm", "run", "serve"]
#CMD tail -f /dev/null
