#FROM julia:latest
FROM julia:1.11.3

ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV PG_HOST=$PG_HOST
ENV PG_PORT=$PG_PORT
ENV PG_USER=$PG_USER
ENV PG_PASS=$PG_PASS
ENV PG_DB=$PG_DB

ARG GENIEAPP_HOST
ENV GENIEAPP_HOST $GENIEAPP_HOST

ARG GENIEAPP_PORT
ENV GENIEAPP_PORT $GENIEAPP_PORT

#ENV GENIE_HOST $GENIEAPP_HOST
ENV GENIE_HOST "0.0.0.0"
ENV PORT $GENIEAPP_PORT
ENV WSPORT $GENIEAPP_PORT

RUN apt-get update && apt-get install -y vim nano
RUN useradd --create-home --shell /bin/bash genie
RUN mkdir /home/genie/app

RUN apt-get update && apt-get install -y procps

WORKDIR /home/genie/app

RUN chown -R genie:genie /home/

USER genie

EXPOSE $GENIEAPP_PORT

ENV JULIA_REVISE_POLL=0

ENV JULIA_DEPOT_PATH "/home/genie/.julia"
ENV GENIE_AUTO_RELOAD "false"
ENV JULIA_REVISE "off"
ENV GENIE_ENV "prod"

RUN julia -e 'using Pkg; Pkg.update(); Pkg.add("Genie"); Pkg.add("LibPQ"); Pkg.add("Tables")'

COPY ./src/app.jl /home/genie/app/app.jl
COPY ./src/db.jl /home/genie/app/db.jl

ENV EARLYBIND "true"
ENV JULIA_CPU_TARGET="generic;sandybridge,-xsaveopt,clone_all;haswell,-rdrnd,base(1)"
RUN julia -e "using Pkg; Pkg.activate(\".\"); Pkg.instantiate(); Pkg.precompile(); "

RUN chmod -R u+w $(find ~/.julia/packages/Genie -type d)

RUN sed -i "230s/.*/if haskey(collection, name)\n    pop!(collection, name)\nend\ncollection[name] = item/" $(find ~/.julia/packages/Genie -name Router.jl)

RUN julia -e 'using Pkg; Pkg.precompile()'

#ENTRYPOINT pkill -9 julia || true && julia --project app.jl
ENTRYPOINT tail -f /dev/null

COPY entrypoint.sh /app/entrypoint.sh
USER root
RUN chmod +x /app/entrypoint.sh
USER genie
#ENTRYPOINT ["/app/entrypoint.sh"]
