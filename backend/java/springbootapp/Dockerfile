FROM maven:3.8.5-openjdk-11

ARG PORT

ARG SPRINGBOOTAPP_PORT
ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV SPRINGBOOTAPP_PORT=$SPRINGBOOTAPP_PORT
ENV PG_HOST=$PG_HOST
ENV PG_PORT=$PG_PORT
ENV PG_USER=$PG_USER
ENV PG_PASS=$PG_PASS
ENV PG_DB=$PG_DB

ENV SPRINGBOOTAPP_PORT_SED_STRING="s/SPRINGBOOTAPP_PORT/${SPRINGBOOTAPP_PORT}/g"
ENV PG_HOST_SED_STRING="s/PG_HOST/${PG_HOST}/g"
ENV PG_PORT_SED_STRING="s/PG_PORT/${PG_PORT}/g"
ENV PG_DB_SED_STRING="s/PG_DB/${PG_DB}/g"
ENV PG_USER_SED_STRING="s/PG_USER/${PG_USER}/g"
ENV PG_PASS_SED_STRING="s/PG_PASS/${PG_PASS}/g"

ENV PORT=$SPRINGBOOTAPP_PORT
ENV NAME="springbootapp"
ENV VERSION="0.0.1-SNAPSHOT"

ENV JAR_FILE=/tmp/target/${NAME}-${VERSION}.jar

RUN mkdir -p /tmp
WORKDIR /tmp/
COPY . .

RUN sed -i $SPRINGBOOTAPP_PORT_SED_STRING src/main/resources/application.properties
RUN sed -i $PG_HOST_SED_STRING src/main/resources/application.properties
RUN sed -i $PG_PORT_SED_STRING src/main/resources/application.properties
RUN sed -i $PG_DB_SED_STRING src/main/resources/application.properties
RUN sed -i $PG_USER_SED_STRING src/main/resources/application.properties
RUN sed -i $PG_PASS_SED_STRING src/main/resources/application.properties

RUN mvn package

# TOGGLE FOR DEBUG:
#ENTRYPOINT ["bash"]

RUN echo "#!/bin/bash\njava -jar ${JAR_FILE}" > /tmp/entrypoint.sh
RUN chmod +x /tmp/entrypoint.sh

EXPOSE ${PORT}
CMD java -jar /tmp/target/NAME-VERSION.jar
ENTRYPOINT ["./entrypoint.sh"]
