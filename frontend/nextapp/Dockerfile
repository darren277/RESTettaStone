FROM node:23-alpine

ARG NEXTAPP_PORT
ENV NEXTAPP_PORT=$NEXTAPP_PORT
ENV PORT=$NEXTAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

ENV NODE_ENV production

RUN apk add --no-cache libc6-compat

RUN npm i -g npm
RUN npm install -g pnpm

WORKDIR /home/nextjs/app

COPY package.json .
RUN touch package-lock.json

RUN pnpm install
RUN pnpm npx update-browserslist-db@latest

RUN npx next telemetry disable

# need to install linux specific swc builds
RUN pnpm install -D @swc/cli @swc/core

COPY ./components ./components
COPY ./pages ./pages
COPY ./styles ./styles

COPY ./config.js ./config.js
COPY ./middleware.js ./middleware.js

RUN echo "NEXT_PUBLIC_NGINX_HOST=$NGINX_HOST" > /home/nextjs/app/.env
RUN echo "NEXT_PUBLIC_NGINX_PORT=$NGINX_PORT" >> /home/nextjs/app/.env

RUN pnpm run build

RUN addgroup -g 1001 -S nodejs
RUN adduser -S nextjs -u 1001

USER nextjs

EXPOSE $PORT

CMD [ "pnpm", "start" ]
