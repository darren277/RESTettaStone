FROM node:latest

ARG PYTHONREACTAPP_HOST
ENV PYTHONREACTAPP_HOST=$PYTHONREACTAPP_HOST

ARG PYTHONREACTAPP_PORT
ENV PYTHONREACTAPP_PORT=$PYTHONREACTAPP_PORT

ARG NGINX_HOST
ENV NGINX_HOST=$NGINX_HOST

ARG NGINX_PORT
ENV NGINX_PORT=$NGINX_PORT

RUN apt-get update && apt-get install -y python3 python3-pip python3-venv git

RUN git clone https://github.com/darren277/Transpiler

WORKDIR /Transpiler

RUN python3 -m venv venv && . venv/bin/activate && pip install --upgrade pip && pip install -r requirements.txt

WORKDIR /Transpiler/tests/testout/multifile
RUN npm install

WORKDIR /Transpiler

EXPOSE $PYTHONREACTAPP_PORT

# These should be changed to environment variables in the original Transpiler repo at some point:
RUN sed -i "s/port: [0-9]\+,/port: $PYTHONREACTAPP_PORT,\n\thost: \"$PYTHONREACTAPP_HOST\",/" tests/testout/multifile/webpack.config.js

RUN mkdir /Transpiler/pythonreactapp
COPY src /Transpiler/pythonreactapp

RUN echo "REACT_APP_NGINX_HOST=$NGINX_HOST" > /Transpiler/tests/testout/multifile/.env
RUN echo "REACT_APP_NGINX_PORT=$NGINX_PORT" >> /Transpiler/tests/testout/multifile/.env

RUN ["/bin/bash", "-c", ". venv/bin/activate && python tests/jsx_tests.py pythonreactapp"]

# Doing the following for now because loading `.env` is not working as desired...
RUN sed -i "s/process.env.REACT_APP_NGINX_HOST/\"$NGINX_HOST\"/" /Transpiler/tests/testout/multifile/src/services/UserService.jsx
RUN sed -i "s/process.env.REACT_APP_NGINX_PORT/$NGINX_PORT/" /Transpiler/tests/testout/multifile/src/services/UserService.jsx

WORKDIR /Transpiler/tests/testout/multifile
CMD ["sh", "-c", "npm start"]
