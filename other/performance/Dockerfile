FROM locustio/locust

ARG TARGET_HOST
ENV HOST=$TARGET_HOST

ARG TARGET_PORT
ENV PORT=$TARGET_PORT

ARG USERS=1
ENV USERS=$USERS

WORKDIR /mnt/locust

COPY ./locustfile.py /mnt/locust/locustfile.py

RUN echo "host = http://$HOST:$PORT" > /mnt/locust/locust.conf
RUN echo "users = $USERS" >> /mnt/locust/locust.conf

ENTRYPOINT locust
