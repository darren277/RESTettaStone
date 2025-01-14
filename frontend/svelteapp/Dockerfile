FROM node:18-alpine AS build

ARG SVELTEAPP_PORT
ENV SVELTEAPP_PORT=$SVELTEAPP_PORT
ENV PORT=$SVELTEAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

WORKDIR /usr/src/app

COPY package.json ./
RUN npm install

COPY ./src ./src
COPY ./index.html ./
COPY ./vite.config.js ./vite.config.mjs

RUN npm run build


FROM nginx:alpine AS runtime

ARG SVELTEAPP_PORT
ENV SVELTEAPP_PORT=$SVELTEAPP_PORT
ENV PORT=$SVELTEAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

RUN rm -rf /usr/share/nginx/html/*

COPY --from=build /usr/src/app/dist /usr/share/nginx/html

EXPOSE $SVELTEAPP_PORT

COPY nginx.conf /etc/nginx/conf.d/default.template.conf

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
