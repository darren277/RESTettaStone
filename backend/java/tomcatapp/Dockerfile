FROM maven:3.9.0-eclipse-temurin-19 as build

ARG PG_HOST
ENV PG_HOST $PG_HOST

ARG PG_PORT
ENV PG_PORT $PG_PORT

ARG PG_DB
ENV PG_DB $PG_DB

ARG PG_USER
ENV PG_USER $PG_USER

ARG PG_PASS
ENV PG_PASS $PG_PASS

ARG FILE_PATH=/files/sample.txt
ENV FILE_PATH $FILE_PATH

WORKDIR /app

COPY pom.xml ./
COPY src ./src

RUN mvn clean package


FROM tomcat:9.0-jdk11-openjdk

ARG TOMCATAPP_PORT
ENV PORT $TOMCATAPP_PORT

ENV CATALINA_HOME /usr/local/tomcat
ENV PATH $CATALINA_HOME/bin:$PATH

RUN curl -o postgresql-42.2.24.jar https://jdbc.postgresql.org/download/postgresql-42.2.24.jar
RUN mv postgresql-42.2.24.jar $CATALINA_HOME/lib/

RUN sed -i "s/port=\"8080\"/port=\"${PORT}\"/" $CATALINA_HOME/conf/server.xml

COPY --from=build /app/target/ $CATALINA_HOME/webapps/
RUN mv $CATALINA_HOME/webapps/tomcat-app-1.war $CATALINA_HOME/webapps/ROOT.war
RUN rm -rf $CATALINA_HOME/webapps/ROOT
RUN rm -rf $CATALINA_HOME/webapps/tomcat-app-1

EXPOSE $PORT

CMD ["catalina.sh", "run"]
