FROM clojure

COPY ./src /usr/src/app/src
COPY ./project.clj /usr/src/app/project.clj
WORKDIR /usr/src/app

ENV PORT=3030

EXPOSE 3030

#CMD ["lein", "run"]
CMD tail -f /dev/null
