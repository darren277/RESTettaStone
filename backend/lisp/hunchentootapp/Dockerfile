FROM debian:latest

ARG HUNCHENTOOTAPP_PORT
ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV PORT $HUNCHENTOOTAPP_PORT
ENV PG_HOST $PG_HOST
ENV PG_PORT $PG_PORT
ENV PG_USER $PG_USER
ENV PG_PASS $PG_PASS
ENV PG_DB $PG_DB

RUN apt-get update && apt-get install -y sbcl curl

WORKDIR /tmp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install :path \"/quicklisp\")" --quit

# Pre-load libraries
RUN sbcl --load /quicklisp/setup.lisp --eval "(ql:quickload '(:hunchentoot :postmodern :cl-json))" --quit

WORKDIR /app

COPY ./src/app.lisp /app/src/app.lisp

CMD sbcl --script /app/src/app.lisp
