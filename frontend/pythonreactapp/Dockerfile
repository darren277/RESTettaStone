FROM node:16

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

CMD ["/bin/bash", "-c", ". venv/bin/activate && python tests/jsx_tests.py"]
