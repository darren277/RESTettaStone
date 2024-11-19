FROM python:3.8

ARG DJANGOAPP_PORT
ENV PORT=$DJANGOAPP_PORT

ARG PG_DB
ENV PG_DB=$PG_DB

ARG PG_USER
ENV PG_USER=$PG_USER

ARG PG_PASS
ENV PG_PASS=$PG_PASS

ENV EMAIL_HOST="localhost"
ENV EMAIL_PORT="25"
ENV EMAIL_HOST_USER=""
ENV EMAIL_HOST_PASSWORD=""

ARG PIP_REQUIREMENTS=production.txt
ENV PYTHONDONTWRITEBYTECODE 1
ENV PYTHONUNBUFFERED 1

# install dependencies
RUN pip install --upgrade pip setuptools

# create user for the Django project
RUN useradd -ms /bin/bash djangoapp

USER djangoapp

WORKDIR /home/djangoapp

COPY --chown=djangoapp ./requirements.txt /home/djangoapp/production.txt

# create and activate virtual environment
RUN python3 -m venv env

RUN env/bin/pip3 install -r $PIP_REQUIREMENTS

COPY --chown=djangoapp ./src /home/djangoapp/djangoapp
COPY --chown=djangoapp ./manage.py /home/djangoapp/manage.py

RUN mkdir -p /home/djangoapp/settings
RUN touch /home/djangoapp/settings/last-update.txt

ENV DJANGO_SECRET_KEY="change-this-to-50-characters-long-random-string"

CMD env/bin/python3 manage.py runserver 0.0.0.0:$PORT
