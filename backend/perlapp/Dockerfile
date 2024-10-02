FROM perl:5.40

ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ARG PERLAPP_PORT

ENV PERLAPP_PORT=$PERLAPP_PORT
ENV PORT=$PERLAPP_PORT

ENV PG_HOST=$PG_HOST
ENV PG_PORT=$PG_PORT
ENV PG_USER=$PG_USER
ENV PG_PASS=$PG_PASS
ENV PG_DB=$PG_DB

WORKDIR /var/www/php

# For caching installation of dependencies...
COPY temp.pl /var/www/php/index.pl

ENV PERL_MM_USE_DEFAULT=1
RUN perl -MCPAN -e 'install DBI'
RUN perl -MCPAN -e 'install MyWebServer'
RUN apt-get update
RUN apt-get install -y libdbd-pg-perl
RUN perl -MCPAN -e 'install DBD::Pg'

# For caching installation of dependencies...
RUN rm -f /var/www/php/index.pl
COPY index.pl /var/www/php/index.pl

CMD ["perl", "index.pl"]
