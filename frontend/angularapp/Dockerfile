FROM node:slim as build

ARG ANGULARAPP_PORT
ENV ANGULARAPP_PORT=$ANGULARAPP_PORT
ENV PORT=$ANGULARAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

ENV NODE_OPTIONS=--openssl-legacy-provider

RUN mkdir /home/app && chown node:node /home/app
WORKDIR /home/app

RUN yarn global add @angular/cli

# the official node image provides an unprivileged user as a security best practice but we have to manually enable it.
# We put it here so npm installs dependencies as the same user who runs the app.
# https://github.com/nodejs/docker-node/blob/master/docs/BestPractices.md#non-root-user
USER node
COPY package.json package-lock.json* ./

RUN yarn install
ENV PATH /home/app/node_modules/.bin:$PATH

# copy in our source code last, as it changes the most
COPY --chown=node:node ./src ./src
COPY --chown=node:node ./angular.json ./
COPY ./tsconfig.app.json /home/app/tsconfig.app.json
COPY ./tsconfig.json /home/app/tsconfig.json

ARG API_BASE_URL="http://${NGINX_HOST}:${NGINX_PORT}/flaskapp"

RUN mkdir -p /home/app/src/environments
RUN echo "export const environment = {\n" > ./src/environments/environment.prod.ts
RUN echo "  production: true,\n" >> ./src/environments/environment.prod.ts
RUN echo "  apiBaseUrl: '${API_BASE_URL}'\n" >> ./src/environments/environment.prod.ts
RUN echo "};" >> ./src/environments/environment.prod.ts

RUN cp ./src/environments/environment.prod.ts ./src/environments/environment.ts

# NOTE that `ng serve` is not meant for production.
# Instead, do `ng build` and then use something like a dedicated Nginx server on this container.
#CMD ng build --configuration production --output-path=/home/app/dist --base-href /angularapp/

# NOTE that this is needed to ensure non interactive execution of `ng serve`.
ENV NG_CLI_ANALYTICS=false

CMD ng serve --host 0.0.0.0 --port 3017 --configuration production
#CMD tail -f /dev/null
