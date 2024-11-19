FROM golang:1.23.1-bullseye

ARG GOAPP_PORT
ENV PORT $GOAPP_PORT

ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV PG_HOST $PG_HOST
ENV PG_PORT $PG_PORT
ENV PG_USER $PG_USER
ENV PG_PASS $PG_PASS
ENV PG_DB $PG_DB

#ENV GO111MODULE=on
#ENV GOFLAGS=-mod=vendor

ENV APP_HOME /go/src
RUN mkdir -p "$APP_HOME"

WORKDIR "$APP_HOME"

COPY ./main.go "$APP_HOME"

RUN go mod init goapp

RUN go get github.com/labstack/echo@latest
RUN go get github.com/labstack/echo/middleware@latest
RUN go get github.com/dgrijalva/jwt-go@latest

RUN go get gorm.io/driver/postgres@latest
RUN go get gorm.io/gorm@latest

EXPOSE $PORT

CMD ["go", "run", "main.go"]
