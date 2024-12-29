
# TOC

- [About](#about)
  * [Parts](#parts)
    + [Database](#database)
  * [History](#history)
  * [Current Status](#current-status)
    + [Backend](#backend)
    + [Frontend](#frontend)
    + [Other](#other)
  * [Standing on the Shoulders of Giants](#standing-on-the-shoulders-of-giants)
  * [Future](#future)
  * [See Also](#see-also)
    + [Theoretical](#theoretical)
    + [Practical](#practical)
- [How to Use](#how-to-use)
  * [Configuring Environment Variables](#configuring-environment-variables)
    + [`.env` File](#env-file)
    + [Setting Environment Variables in Redis and Nginx](#setting-environment-variables-in-redis-and-nginx)
  * [Using the Makefile](#using-the-makefile)
    + [Overview of Commands](#overview-of-commands)
      - [Docker](#docker)
  * [Using Docker Compose](#using-docker-compose)
  * [Testing](#testing)
    + [Simple Endpoint Testing with `Make`](#simple-endpoint-testing-with-make)
    + [Using Python Script for More Advanced Testing](#using-python-script-for-more-advanced-testing)
  * [Performance Testing with Locust](#performance-testing-with-locust)
  * [Monitoring](#monitoring)
    + [Prometheus and Grafana](#prometheus-and-grafana)
  * [Cloud Deployment](#cloud-deployment)
    + [AWS](#aws)
      - [EC2](#ec2)
        * [Some Docker Notes](#some-docker-notes)
          + [Docker Permissions](#docker-permissions)
          + [Testing Endpoint with Curl](#testing-endpoint-with-curl)
        * [Some Notes About Make and SSH Commands](#some-notes-about-make-and-ssh-commands)
        * [SSH Keys](#ssh-keys)
          + [Key File Permissions](#key-file-permissions)
          + [Key Format](#key-format)
  * [RabbitMQ](#rabbitmq)
- [Lessons](#lessons)
  * [DevOps Gotchas](#devops-gotchas)
    + [Nginx Gotchas](#nginx-gotchas)
      - [Environment Variables](#environment-variables)
    + [Docker Gotchas](#docker-gotchas)
      - [ARGS and Multi Stage Builds](#args-and-multi-stage-builds)
      - [Exec Form vs Shell Form for ENTRYPOINT:](#exec-form-vs-shell-form-for-entrypoint-)
    + [Docker Compose Gotchas](#docker-compose-gotchas)
      - [Dynamic Build Args](#dynamic-build-args)
    + [Make Gotchas](#make-gotchas)
      - [Tabs vs Spaces](#tabs-vs-spaces)
  * [Language and Framework Gotchas](#language-and-framework-gotchas)
    + [Symfony (PHP)](#symfony--php-)
      - [Headers vs Body and the Whitespace Issue](#headers-vs-body-and-the-whitespace-issue)
      - [CLI Race Condition](#CLI-Race-Condition)
    + [Rocket (Rust)](#rocket--rust-)
      - [Pear Codegen Dependency](#pear-codegen-dependency)
    + [Django (Python)](#django--python-)
      - [Project Structure](#project-structure)
      - [URL Routing](#url-routing)
      - [Django REST Framework](#django-rest-framework)
    + [Zig](#zig)
      - [Postgres Connection Pool Inconsistency Problem](#postgres-connection-pool-inconsistency-problem)
    + [Fat Free (PHP)](#fat-free--php-)
      - [Dependency Source](#dependency-source)
  * [Database Gotchas](#database-gotchas)
    + [Postgres](#postgres)
      - [pg_stat_activity Locks](#pg_stat_activity-locks)
  * [Comparisons](#comparisons)
    + [Projects by Language](#projects-by-language)
      - [Classic](#classic)
      - [Performant](#performant)
      - [Procedural](#procedural)
      - [Object Oriented](#object-oriented)
      - [Functional](#functional)
      - [Multi-Paradigm](#multi-paradigm)
      - [Versatile or Other](#versatile-or-other)
      - [JavaScript](#javascript)
    + [Metrics](#metrics)
      - [Qualitative](#qualitative)
        - [Simplicity](#simplicity)
      - [Quantitative](#quantitative)
        - [Performance](#performance)
      - [Both Qualitative and Quantitative](#both-qualitative-and-quantitative)
        - [Security](#security)
        - [Safety](#safety)
- [Optimization](#optimization)
  * [Docker Image Size](#docker-image-size)
    + [Table Sorted Alphabetically](#table-sorted-alphabetically)
    + [Table Sorted by Image Size](#table-sorted-by-image-size)

# About

This is a project I started back in Fall of 2022. The purpose is to have a repository showcasing basic REST API functionality, Dockerized, for a multitude of programming languages and frameworks.

## Parts

This repository consists of a `backend` and `frontend` folder, each in turn consisting of subdirectories for each framework or server implementation.

I also include a `Dockerfile` inside of each subdirectory so that each mini app can be run in its own container.

Furthermore, I use Nginx to route requests to each backend.

There is also a `Makefile` with the commands to build and run each container, as well as some general purpose commands for creating the network, handling logs and simple container interaction tests, etc.

### Database

This project uses a single Postgres database for all backends to interact with.

## History

I started this back in 2022 and made a lot of headway with it in the beginning, but ended up putting it aside in November of 2022 as I just started a new job and simply did not have the time to continue adding to it.

I found myself dusting it off a couple of times, briefly, during 2024.

I've decided to set out to complete as much of what's left as I can now, hoping to maybe finish it before the year is out.

## Current Status

This table represents the status of the project as of November 2022.

I'm leaving the Python options (Flask and Django) until last as they will be by far the easiest to implement for me.

I break down the barriers I've faced into a few fundamental categories:
* **Language**: A lack of experience with the language and some of its specific nuances. This barrier is especially prevalent with languages that use non-object-oriented paradigms (for example, Lisp, Haskell, Prolog, etc).
* **Libraries**: Since I'm using Postgres for the universal database that all the backends are accessing, I need an existing library or implementation for accessing a Postgres database. I think the only place where this was a total blocker was Bun.js. I started looking into the feasibility of writing my own super simple implementation, but abandoned that due to a lack of available time. It turns out, after a couple years have passed, that there is now Postgres support for Bun.js.
* **Networking**: This is a case where I have difficulties accessing the endpoint for some reason. I recall a particular scenario where `0.0.0.0` was not doing what it was supposed to be doing, and I just hit a wall with it and gave up at the time. Looking at the table, it looks like that was one of the Rust frameworks. I plan to revisit that and troubleshoot it further.
* **Build**: This is where the Dockerfile fails to execute every step and the application simply does not run. It looks like Zig was the only such scenario. This is another one I will try revisiting after all this time.

Note that I use `Y` to indicate that the task is completed, tested, and merged into `main`. I use `'` to indicate that the task was completed at some point (likely two years ago), and is going through a testing stage before a PR is made.

I also have two asterisks (`*`) but I can't remember what those were for. I'm leaving them in for now.

### Backend

|                                                                                                        | Dockerized | REST Responses | Postgres Interaction | Full CRUD | Current Barrier |
|--------------------------------------------------------------------------------------------------------|------------|----------------|----------------------|-----------|-----------------|
| **COBOL**                                                                                              |            |                |                      |           |                 |
| Cobol                                                                                                  | '*         | '              |                      |           | Language        |
| **C++**                                                                                                |            |                |                      |           |                 |
| [Crow (C++)](https://github.com/darren277/RESTettaStone/tree/master/backend/cpp/crowapp)               | Y          | Y              | Y                    | Y         |                 |
| **C#**                                                                                                 |            |                |                      |           |                 |
| [Asp.Net (C#)](https://github.com/darren277/RESTettaStone/tree/master/backend/csharp/aspnetapp)        | Y          | Y              | Y                    | Y         |                 |
| **FORTRAN**                                                                                            |            |                |                      |           |                 |
| Fortran                                                                                                |            |                |                      |           | Language        |
| **F#**                                                                                                 |            |                |                      |           |                 |
| [F#](https://github.com/darren277/RESTettaStone/tree/master/backend/fsharp/fsharpapp)                  | Y          | Y              | Y                    | Y         | ~~Language~~    |
| **GO**                                                                                                 |            |                |                      |           |                 |
| [Go](https://github.com/darren277/RESTettaStone/tree/master/backend/go/goapp)                          | Y          | Y              | Y                    | Y         |                 |
| **HASKELL**                                                                                            |            |                |                      |           |                 |
| [Spock](https://github.com/darren277/RESTettaStone/tree/master/backend/haskell/spockapp)               | Y          | Y              | Y                    | Y         | Language        |
| **JAVA**                                                                                               |            |                |                      |           |                 |
| [Tomcat (Java)](https://github.com/darren277/RESTettaStone/tree/master/backend/java/tomcatapp)         | Y          | Y              | Y                    | Y         |                 |
| [SpringBoot (Java)](https://github.com/darren277/RESTettaStone/tree/master/backend/java/springbootapp) | Y          | Y              | Y                    | Y         |                 |
| **JAVASCRIPT**                                                                                         |            |                |                      |           |                 |
| [Bun (JS)](https://github.com/darren277/RESTettaStone/tree/master/backend/javascript/bunapp)           | Y          | Y              | Y                    | Y         | ~~Libraries~~   |
| [Firebase (JS)](https://github.com/darren277/RESTettaStone/tree/master/backend/javascript/firebaseapp) | Y          | Y              | N/A                  | Y         |                 |
| [Node (JS)](https://github.com/darren277/RESTettaStone/tree/master/backend/javascript/nodeapp)         | Y          | Y              | Y                    | Y         |                 |
| **LISP**                                                                                               |            |                |                      |           |                 |
| Lisp                                                                                                   |            |                |                      |           | Language        |
| **LUA**                                                                                                |            |                |                      |           |                 |
| [Lua / OpenResty](https://github.com/darren277/RESTettaStone/tree/master/backend/lua/luaapp)           | Y          | Y              | Y                    | Y         |                 |
| **PASCAL**                                                                                             |            |                |                      |           |                 |
| Pascal                                                                                                 | '          | '              | '                    |           |                 |
| [Perl](https://github.com/darren277/RESTettaStone/tree/master/backend/perl/perlapp)                    | Y          | Y              | Y                    | Y         |                 |
| **PHP**                                                                                                |            |                |                      |           |                 |
| [Fat Free (PHP)](https://github.com/darren277/RESTettaStone/tree/master/backend/php/fatfreeapp)        | Y          | Y              | Y                    | Y         |                 |
| [Laravel (PHP)](https://github.com/darren277/RESTettaStone/tree/master/backend/php/laravelapp)         | Y          | Y              | Y                    | Y         |                 |
| [PHP](https://github.com/darren277/RESTettaStone/tree/master/backend/php/phpapp)                       | Y          | Y              | Y                    |           |                 |
| [Symfony (PHP)](https://github.com/darren277/RESTettaStone/tree/master/backend/php/symfonyapp)         | Y          | Y              | Y                    | Y         |                 |
| **PROLOG**                                                                                             |            |                |                      |           |                 |
| [Prolog](https://github.com/darren277/RESTettaStone/tree/master/backend/prolog/prologapp)              | Y          | Y              | Y                    | Y         | ~~Language~~    |
| **PYTHON**                                                                                             |            |                |                      |           |                 |
| [Django (Python)](https://github.com/darren277/RESTettaStone/tree/master/backend/python/djangoapp)     | Y          | Y              | Y                    |           |                 |
| [Flask (Python)](https://github.com/darren277/RESTettaStone/tree/master/backend/python/flaskapp)       | Y          | Y              | Y                    | Y         |                 |
| **RUBY**                                                                                               |            |                |                      |           |                 |
| [Rails (Ruby)](https://github.com/darren277/RESTettaStone/tree/master/backend/ruby/railsapp)           | Y          | Y              | Y                    | Y         |                 |
| **RUST**                                                                                               |            |                |                      |           |                 |
| [Actix (Rust)](https://github.com/darren277/RESTettaStone/tree/master/backend/rust/actixapp)           | Y          | Y              | Y                    | Y         | ~~Language~~    |
| [Rocket (Rust)](https://github.com/darren277/RESTettaStone/tree/master/backend/rust/rocketapp)         | Y          | Y              | Y                    |           | ~~Networking~~  |
| **SCALA**                                                                                              |            |                |                      |           |                 |
| [Play (Scala)](https://github.com/darren277/RESTettaStone/tree/master/backend/scala/playapp)           | Y          | Y              | Y                    | Y         |                 |
| **SWIFT**                                                                                              |            |                |                      |           |                 |
| [Swift](https://github.com/darren277/RESTettaStone/tree/master/backend/swift/swiftapp)                 | Y          | Y              | Y                    | Y         |                 |
| **D**                                                                                                  |            |                |                      |           |                 |
| [Vibe (D)](https://github.com/darren277/RESTettaStone/tree/master/backend/vibe/vibeapp)                | Y          | Y              | Y                    | Y         | ~~Networking~~  |
| **ZIG**                                                                                                |            |                |                      |           |                 |
| [Zig](https://github.com/darren277/RESTettaStone/tree/master/backend/zig/zigapp)                       | Y          | Y              | Y                    | Y*        | ~~Build~~       |

### Frontend

|                                                                                              | Dockerized | Backend Integration | Full CRUD |
|----------------------------------------------------------------------------------------------|------------|---------------------|-----------|
| [Angular](https://github.com/darren277/RESTettaStone/tree/master/frontend/angularapp)        | Y          | Y                   | Y         |
| [Gatsby](https://github.com/darren277/RESTettaStone/tree/master/frontend/gatsbyapp)          | Y          | Y                   | Y         |
| [Next](https://github.com/darren277/RESTettaStone/tree/master/frontend/nextapp)              | Y          | Y                   | Y         |
| [React](https://github.com/darren277/RESTettaStone/tree/master/frontend/reactapp)            | Y          | Y                   | Y         |
| [React Fiber](https://github.com/darren277/RESTettaStone/tree/master/frontend/reactfiberapp) | Y          | Y                   | Y         |
| [Vue](https://github.com/darren277/RESTettaStone/tree/master/frontend/vueapp)                | Y          | Y                   | Y         |

### Other

|                                                                                             | Dockerized | Backend Integration | Full CRUD |
|---------------------------------------------------------------------------------------------|-----------|---------------------|-----------|
| [Electron](https://github.com/darren277/RESTettaStone/tree/master/other/electronapp)        | N/A       | Y                   | Y         |
| [Expo / React Native](https://github.com/darren277/RESTettaStone/tree/master/other/expoapp) | N/A       | Y                   | Y         |
| [Chalice](https://github.com/darren277/RESTettaStone/tree/master/other/chaliceapp)          | N/A       | N/A                 |           |
| [Monitoring](https://github.com/darren277/RESTettaStone/tree/master/other/monitoring)       | Y         | Y                   |           |
| [gRPC](https://github.com/darren277/RESTettaStone/tree/master/other/grpc)                   | Y         | N/A                 |           |
| [RabbitMQ](#rabbitmq)                                                                       | Y         | N/A                 |           |

### Servers

|                                                                                      | Dockerized | Backend Integration | Frontend Integration |
|--------------------------------------------------------------------------------------|------------|---------------------|----------------------|
| [Nginx](https://github.com/darren277/RESTettaStone/tree/master/server/nginx)         | Y          | Y                   | Y                    |
| [Apache Httpd](https://github.com/darren277/RESTettaStone/tree/master/server/apache) | Y          | Y                   | Y                    |

## Standing on the Shoulders of Giants

In many cases, I made use of some existing projects out there as templates or boilerplate. I wound up modifying many of the Dockerfiles I found, for example, quite substantially, to suit my custom needs. There were also a few repositories I found on GitHub that helped guide my initial efforts for some of the languages where I significantly lacked direct experience.

One major example of this was the [COBOL backend](https://github.com/azac/cobol-on-wheelchair). I'd have had no idea where to even begin for something like that (well, other than the language specifications and documentation, I suppose - but that would have been unecessarily time consuming, and why reinvent the wheel?).

## Future

The first immediate steps to carry out will be gradually adding each subdirectory as I test them and maybe clean them up a bit by removing unecessary comments and so on.

## See Also

What follows in this section are some other projects I have worked on that are at various stages of completion. They relate to this one in that they explore other aspects of web and desktop application development that do not necessarily fit directly into this repository, or are simply much bigger projects and deserve to stand in their own right.

### Theoretical

* [Series of tubes](https://github.com/darren277/series-of-tubes) is a repository where I analyze the underlying structure of Internet protocols and various constituent parts that compose the various kinds of Internet traffic that make up the World Wide Web. It is a work in progress with many more parts to come.
* [byodb](https://github.com/darren277/byodb) is an implementation of a SQLite like database from the ground up in C. It is based on a tutorial referenced inside the repository.

### Practical

* [Python to JS/JSX Transpiler](https://github.com/darren277/Transpiler) is a project for transpiling Python code into either JavaScript or JSX code. Theoretically, it could potentially be leveraged to write entire React applications in Python.
* [Wasm-FRP](https://github.com/darren277/wasm-frp) is a project that uses Rust at every level of the stack.
  - The back end uses Tokio to construct a REST based web server.
  - The front end uses Yew to construct a WebAssembly based front end that is compiled and then served client side.
  - The database it interacts with is even a Rust based technology called SurrealDB. It is a multimodal database with a lot of fun potential.

# How to Use

## Configuring Environment Variables

### `.env` File

You will need a `.env` file in the root directory with quite a few custom defined environment variables.

The universal environment variables for the central control mechanism(s) would look something like this:

```sh
PG_USER=someusername
PG_PASS=somepassword

NGINX_VERSION=1.17.3
NGINX_IP=172.18.0.20
NGINX_PORT=8084
LUA_JIT_VERSION=2.0.5

SUBNET_NAME=restetta
SUBNET_CIDR=172.18.0.0/16

PG_IP=172.18.0.21
PG_HOST=172.18.0.21
PG_PORT=5432
PG_DB=postgres

REDIS_IP=172.18.0.19
REDIS_PORT=6379
```

And then individual environment variables for each subproject would look something like this:

```sh
CROWAPP_IP=172.18.0.22
CROWAPP_PORT=18080
```

Please note that a lot of these are quite arbitrary. The CIDR subnet that you use, and the IP addresses for each Docker container can almost be randomly generated. The same applies to the ports.

### Setting Environment Variables in Redis and Nginx

The Nginx configuration process here currently requires knowledge of each container's IP address and port.

You'll notice the following command in the `Makefile`:

```sh
redis-set-variable:
    docker exec -it redis_container redis-cli set $(REDIS_KEY) $(REDIS_VALUE)
```

On Windows (for example), you simply need to open a terminal and execute the following type of command (using a particular illustrative example):

```shell
$env:REDIS_KEY='CROWAPP_PORT'; $env:REDIS_VALUE='18080'; make redis-set-variable
```

Take note, as well, of the following command inside the `server/nginx/entrypoint.sh` script:

```sh
envsubst '${NGINX_PORT},${CROWAPP_IP},${CROWAPP_PORT}' < /usr/local/openresty/nginx/conf/nginx.conf.template > /usr/local/openresty/nginx/conf/nginx.conf
```

I will be adding each environment variable pair (IP and port) for each subproject as I add them to the repository.

This will, of course, become rather long and cumbersome over time, so I will likely implement a more elegant solution in the future.

## Using the Makefile

The Makefile, which requires installation of `make` if you do not already have it, is provided to facilitate the execution of many of the commands needed to run the different pieces of this project.

## Using Docker Compose

**NOTE**: I've abandoned this route as I absolutely refuse to hardcode the build args directly inside of the `docker-compose.yml` file.

For a project of this size, with dozens of subprojects each with their own `Docker` container specifications, it just doesn't make sense if Docker Compose won't accommodate dynamic build arg interpolation.

See also: [The build arg gotcha](#docker-compose-gotchas).

For the sake of posterity, I still include the existing `docker-compose.yml` file in the root directory for anyone who would like to build upon it.

If you'd like to try using the `docker-compose.yml` file, you can do so by running the following command from the CLI:

```shell
docker-compose up
```

Two things to note:
1. I have not yet added every single subproject to the `docker-compose.yml` file. This would be rather tedious, very repetitive, and essentially unnecessary for the time being. The `Makefile` and each individual `Dockerfile` do the job just fine.
2. Since the YAML keys cannot be interpolated with environment variables, I had to hardcode the network name which bothers me in an OCD kind of way. It's obviously not a huge deal, but it irks me just a smidge.

See also: [Build args gotcha](#docker-compose-gotchas).

### Overview of Commands

#### Docker

Most of the commands in the Makefile(s) are calling the `docker` CLI tool.

This includes tasks such as creating the network, building images from individual `Dockerfile`s, running these Docker images, and, in some cases, interacting with them.

I also recently separated the original single `Makefile` into two separate files (with a third to come - namely, `Makefile.frontend`).

To run a command from the main central `Makefile`, you simply enter something like the following into the terminal:

```shell
make docker-subnet
make docker-psql
make nginx-build
make nginx-run
```

As for the `Makefile.backend`, the recurring pattern looks as follows:

```shell
make b actixapp-build
make b actixapp-run
```

Or:

```shell
make b cobolapp-build
make b cobolapp-run
```

Note that the `b` is a shorthand I created to simplify referencing the `Makefile.backend` file.

Don't forget to define your environment variables in a `.env` file, as they are used by these `Makefile` commands. See above section on environment variables for details.

## Testing

### Simple Endpoint Testing with `Make`

I created a Dockerfile for a new container that can be used for testing endpoints.

I've also included one example usage:
```shell
crowapp-test:
	docker exec -it debugger curl http://$(CROWAPP_IP):$(CROWAPP_PORT)/users
```

The next thing I'd like to add to this generalized debugger container is a logging system that can create logs either on the host machine or at least be very easily accessible via another `Make` command.

Or, better yet, perhaps create some kind of visual user interface for viewing these logs (or leveraging something like `Kibana` or `Grafana`).

### Using Python Script for More Advanced Testing

I've also included a [Python script](https://github.com/darren277/RESTettaStone/blob/master/other/tests/main.py) that can be used to define some more advanced test cases, including testing for specifics about the returned content.

This script also allows you to test every container with the same command:
```shell
make run-tests
```

## Performance Testing with Locust

I've added a container definition and Locust file and configuration options for performance testing.

There are `make` commands in the `Makefile`:

```shell
locust-build:
	cd other/performance && docker build --build-arg TARGET_HOST=$(TARGET_HOST) --build-arg TARGET_PORT=$(TARGET_PORT) -t locust-$(NAME):1 .

locust-run:
	docker run -d -it --rm -p 8089:8089 --name locust-$(NAME) --network $(SUBNET_NAME) locust-$(NAME):1
```

You can manually run the `docker build` command as follows:
```shell
locust-build NAME=crowapp TARGET_HOST=127.420.69.42 TARGET_PORT=2024
```

And then run the container with the following command:
```shell
locust-run NAME=crowapp
```

**_Or_**, more preferably... You will likely want to define your own tailored `Make` commands, following the given Flask example already in the `Makefile`:

```shell
locust-build-flask:
	$(MAKE) locust-build NAME=flask TARGET_HOST=$(FLASKAPP_IP) TARGET_PORT=$(FLASKAPP_PORT)

locust-run-flask:
	$(MAKE) locust-run NAME=flask
```

I may add a separate `Makefile` in the future that is defined to calling all the different performance testing configurations.

In the meantime, however, I will leave that as an exercise for the reader.

## Monitoring

### Prometheus and Grafana

There is a `docker-compose.yml` file in the `other/monitoring` directory that can be used to start a Prometheus monitoring and alerting system with Grafana configured for some very simple dashboard visuals.

To access the Prometheus dashboard, navigate to `http://localhost:9090`.

To access the Grafana dashboard, navigate to `http://localhost:3050`.

The username is `admin`, and the password is set as an environment variable in the root `.env` file as `GF_SECURITY_ADMIN_PASSWORD`.

It is currently connected to the `flaskapp` Flask back end. I will leave connecting it to other sub projects as an exercise for the reader.

Note: Unlike the root project and it's `Makefile` configurations, the ports and other configuration values are currently hardcoded, which I don't particularly like, but I'll likely revisit that later. Also be aware that, currently, the volume mount for the Prometheus container is the same as the source folder, so it currently modifies the source `prometheus.yml` file each time it's run. This is not ideal, but it's another thing I'm leaving for another day.

## Cloud Deployment

### AWS

#### EC2

This procedure is going to be making a lot of assumptions regarding preparatory work on the part of the reader. There are lots of resources out there on how to do these things, so I won't add to the noise out there with my own take on an oft repeated process.
1. Sign up for an AWS account.
2. Provision an EC2 instance that is within your budget.
3. Install and configure Docker on this EC2 instance.
4. Add all required environment variables to your `.env` file (see `Makefile` for reference).
5. Install the `aws` CLI tool locally.
6. Configure the `aws` CLI tool with your AWS credentials.
7. Install and configure the necessary SSH tools on your local machine.
8. Obtain the right private key file for your EC2 instance security group and point to it using `PEM_KEY_PATH` in the `.env` file.
9. Ensure that Postgres is installed and running on the EC2 instance `sudo apt install postgresql`.
10. Ensure that you have Python installed on the EC2 instance `sudo apt install python3`.
11. And also ensure you have the `python3-dev` package installed `sudo apt install python3-all-dev`.
12. And yet another prerequisite for Postgres is `sudo apt install libpq-dev`.

An alternative to the last four steps would be to slightly modify the `populate_pg_db.py` script to run locally while connecting to the remote Dockerized Postgres database. I'll leave this as an exercise for the reader for the time being.

##### Some Docker Notes

###### Docker Permissions

If you face any Docker permissions issues right from the get go, there are a few ways to go about resolving that.

The method I went with was: `sudo chown $USER /var/run/docker.sock`.

Be warned that this particular method carries some degree of security risk.

To test: `docker run hello-world`.

It may require a `reboot`, but if `reboot` is not ideal, you can restart the Docker service with `sudo systemctl restart docker`, or if that doesn't work, use `sudo systemctl list-units --type=service` to list the services and find the Docker service name. In my case, it was `snap.docker.dockerd.service` so I had to run `sudo systemctl restart snap.docker.dockerd.service`.

See also: https://stackoverflow.com/questions/48957195/how-to-fix-docker-got-permission-denied-issue

###### Testing Endpoint with Curl

This step also has a couple of prerequisites:
1. You must of course have `curl` installed locally.
2. You must have the IP address or hostname of your EC2 instance stored in your `.env` file as `EC2_HOST`.
3. You must also have the desired port number allowed access via the security group settings for that EC2 instance (configured via AWS Console, or from the CLI if you're familiar with the process).

##### Some Notes About Make and SSH Commands

There are likely better equipped tools out there for accomplishing a lot of what I am doing with a `Makefile` and a few SSH related commands.

Ansible, although my experience with it is limited so far, sounds like it might be a good fit for this kind of thing. I'll be exploring that in a future iteration.

##### SSH Keys

###### Key File Permissions

If you happen to encounter something like the following:
```
The authenticity of host 'ec2-X-X-X-X.compute-1.amazonaws.com (X.X.X.X)' can't be established.
ED25519 key fingerprint is SHA256:rZEy5qUlf9lEj3pDN6b2mWx9XEzilKDkeykF/tpuZKL.
This key is not known by any other names.
Are you sure you want to continue connecting (yes/no/[fingerprint])?
Warning: Permanently added 'ec2-X-X-X-X.compute-1.amazonaws.com' (ED25519) to the list of known hosts.
Bad permissions. Try removing permissions for user: NT AUTHORITY\\Authenticated Users (S-1-5-11) on file C:/Users/Darren/.ssh/my_key.ppk.
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@         WARNING: UNPROTECTED PRIVATE KEY FILE!          @
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
Permissions for 'C:/Users/Darren/.ssh/my_key.ppk' are too open.
It is required that your private key files are NOT accessible by others.
This private key will be ignored.
Load key "C:/Users/Darren/.ssh/publicwebsite.ppk": bad permissions
ubuntu@ec2-X-X-X-X.compute-1.amazonaws.com: Permission denied (publickey).
C:\Windows\System32\OpenSSH\scp.exe: Connection closed
```

You may have to modify the permissions for the file to be more exclusive to a particular user.

There is a discussion with some suggestions and guidance on how to do that here: https://stackoverflow.com/questions/49926386/openssh-windows-bad-owner-or-permissions

Just keep in mind the usual caveats when it comes to fiddling with file permissions.

###### Key Format

The key format needed for the `Makefile` commands that use SSH from the command line need to be in the following format:
```shell
-----BEGIN RSA PRIVATE KEY-----
UAixppRe3O2ib7z7R9bsYFAjnMG+hMJ4qxTSZUcNaa0rzPnEtmNeCdprl6Ta2f/2t8+l3TYRthXyI
+TBwFYZ0l/Bo5dJ1J9uZ5wXy94ce0TdCZIlPClYP7xXvGnf23OsNgsV2aZf3sr1llgLTepAbRjuXU
K5tyzRaAYH+O4S3+D9t3guGyFMZlF8mZNux4LrcRYMn0czmA3gjS23Fj7IFhZtaBO95jw2Mh3cRYl
qiiqGtt/+j1lsqDvztwpxdrE/jWMnXLDMptWq/BCyVYp1uzIBm0gRwGvXsv+yj+OFhJ9l84P2vKCM
du5HW3mdIlX5ivpdepPWENvi3ubzrW80fzcs3idHMqoXVguf+go/nS2FwFaJLUhm55IDr0EmB8hZB
b8JGRlX8zSnkOXUQb7vlmk4PpoAINbEzedKVEct06Gt7vGbNRNnyha3h0sW6N3FXYyQjeANwHHxNU
1z1yWWkWUlnne8h5N3B52mNjBZgXI6JhWNxgTvUWM5GmsxPHeciqS7sXYyyNUA+Nlad2fDiBOEirC
z7bpU9hVgEVNXnYBzzhgMHe44lTV8rwRXK28JCsdmO00wunxrDvkGKfznwqqCWFHEBCUEzsXYDetx
t+iQsvG0kY7dAgkwvR/b+FiW03OVbap4IK3UWkYJgwwiy1Qu3iPGBTFE3KzM6HSPRTEfH2K5GKjD1
TuCK4SU/H75c4imbFExawWa1w2o69PdxOU8Gv3f4nwfEwN4bNgkpNajBmLyII2pq4j8I9G+vLQUso
EnL7XrQzkQse99YYf8FGQsZG/umgRc7Ggxzjfsj34GWehnupE4Q3b5hJNh0qC6F919Q9NJqJ9y3hh
XEOMsmAeGC3XxMxoByn2cG+G/7UQnCamN7vzru9qKo37vwcPhOu45CIpml88bZ9CUa4AKpjLXuEym
aXU5bC8mlK0ZcCSTN/WvcuM1bXo/uMx8O8RIzf6ilbtQ6nzMiNVx0tTh4YFGRX2CtrKIzxQIRqrLp
zz/uPGvR1w7YbRFaUSmqSTqBdbetpvszo6qld/7dK2rT/LAF4oUAzD130wvD8TnxbY5U4kkFgo51y
ZUoeKrlXdRZgFa26EQjV5GYVjsciVb9kjcbjAYRaIN/ErcEycliFvFblEL7D2WSUKMybJh8y7ZuxG
6G8UE8X3DPbWDqZ7WBlz8GHgnRATID24GpCYZlW3sTuBiHfRasvhijA8w/5tjN+71cdrF977gmEyD
3MRlDEXT9ZxT9SHEjsWD/AlPf7UGyvzAP0M5r70WMeQROXV1oI8Ilk5yqVKGmAvbrgBiVOvy6zerh
1d9bZVNsim0IrsdqtgG5DTH5gXDZ+aNOKIKCYAvFIuSQoDufxNr4j88XaO6D21D0dSsaHCug1a3pm
QOAgxGEsvTh6nlXYK72ZNXkYYCiZiYM3+ukhdDSPGnpXkY6tmh9pNbYvmM4kAYvyfh8dfClImG/HA
sii3ECAGfZE3rNddvuyhUdR6FyOduKMHyp7MvMF5u2QO9h0oG/nBKw5HgYCDl3g8WLlhqq50Lq9iy
Ic5iLbk1/Bon5VKG8LOs/MJyIFA8TnpEgUZ2afd38ZwpPpckXitTmXtFFtgze26LGqOpiKB=
-----END RSA PRIVATE KEY-----
```

Not this format:
```shell
PuTTY-User-Key-File-2: ssh-rsa
Encryption: none
Comment: imported-openssh-key
Public-Lines: 6
7FQOxsna4r3cvUluR3IlvYYmcblZcO06deS0auu3j5xTBr2G3+JrFpxsPPVLWR6lJ
jUhAcjsfJuUeToeCIubdrVWX4kbFvfjLm0ZylkJw6lnC1tsugPE6KnjUv4TnOMNZJ
mxPXoQUr/X22gHuQgcycG65jgwZ9NS000YW4F22DcUHvgibiwytPYThberPczi8zo
QSKi0HSQ6OgoDfzy5o7Lj8HB5cyS4HIQ8t6ckiPv9EXQMoqGbvEjHJJqBOJYX1uO8
NG6MWBrZEy5qUlf9lEj3pDNCLGK6b2mWx9XEzilKDkekvyykFL/tpuZKLfAPDUtF5
yJLAUpcGEj96/RUdYvCvqONzDSllTOq5iRPBIVIpqTqfeqU8f+vx
Private-Lines: 14
cOwOdky2eprrY5HmskdNERB0Q3G8AFb+fAwCnv8ijUddw/PtLrpthFflaxoSWfubT
70NzZ5Q85He6o/b4ciEMVVor5+P+NrzBMYap3MigAykBEkW9eN/v//WnZ8C/YaNhs
Xwq8dCJA4uWVnWYPRuPjI+n56IPYw+0Yf/eWQsES+2CO8Ut9A9gn+i09ziZrRzFOT
79rdwTVjY77KBqxbL/yjHZ/nEBzXWUT9EI2aXWHdDcPrKGP9K1NpTAef8KKQ4S/0j
pO0Sh9j2BKIDdvMn0xvkXCuxcbp0Hb9IpVQMmjF+Hnp+3EA5MUvGJZMMgCr/Fvhqn
W/FrFtbYrqdItJMb6aXbBCPQhATPNSu4B0w9QSGvmvJEEtu0Uv2nnKZX2tupVL7tq
KYLMo/iB/Qcq4y36ANEVfw/t7js47kFCNe5ERPVnR4/vXkhzmRZ7DiuHr6+Y1xWw4
1G1CVKNJwQZchuQy+cdX2YfFVk6pUCIcsl928zzV/7BSEBfnfRB/0X1iJbkLLoWpc
dP2v6wI4Ey9eTCFKs7iSvqwhI2DxL0WjwNo9SLZxip8joEQoLUWGaMwTxQ52TjfQB
0QQ1CtRYYgPt8TZD6wSdnBrm5s1SsZr4L4DDRPDONbbbTCDvpaarS24AqDd++iOS0
ifV4OXD1nzMlSxh6o9zUeF/sXvlHdH+g99K4nyQJnRIG1VkTIYMeghNO2GZkRwVUI
bxXiJsM6iwXnvtlDLmNyIw7RdLCQQgt1yL72VZzwcATjPy6D8E7cpvXZctLoSDqjV
ZPWzm0TNfBWqbg/t379hgSsU0cI2QnN8684Z6gLOvm4xTUx6LQOsrWQrF0irQCWPD
oyx6vgyLpS37kVhrRnnx093cXzFYzhfSl8RN5zSur/JY/LD=
Private-MAC: f6624c2b7933e85f536d0020fe519821074a415c
```

There are conversion tools out there. I believe [PuTTYgen](https://www.puttygen.com) does the trick.

By the way, do not share your private keys in public places. The above two examples are not actual key files of mine, but rather generated with a script to look nearly identical to actual key files of each type.

And why would I go to the trouble of writing a script to do such a thing?

Well, [why not](https://github.com/darren277/RESTettaStone/tree/master/other/utils/just_for_fun/keygen.py)?

## RabbitMQ

In a browser, navigate to http://localhost:8080 and enter `guest` for both the username and password.

There are some `Makefile` commands for RabbitMQ as well.

To build and run the RabbitMQ container, you can use the following commands:
```shell
make mq run
```

To test the container, run the following command:
```shell
make mq test
```

To interact with the RabbitMQ container, there are quite a few commands in the `Makefile`, such as:
```shell
make mq create-queue queue=my_queue
make mq publish-message queue=my_queue msg="Hello, World!"
```

# Lessons

## DevOps Gotchas

### Nginx Gotchas

#### Environment Variables

When trying to assign a variable port to the `listen` directive, I discovered that Nginx simply does not allow for that, even when using leveraging lua.

I had to use an `envsubst` call in the Dockerfile as a work around.

### Docker Gotchas

#### ARGs and Multi Stage Builds

I kept ending up with empty strings for my ENV vars when trying to construct variable rich strings. It turns out that build `ARG` values passed in via the Docker CLI are reset with each build when it comes to multi stage builds.

Refer to: https://stackoverflow.com/a/68061457/10973023

#### Exec Form vs Shell Form for ENTRYPOINT:

When including ENV vars inside of a `Dockerfile`, it is important to note that `ENTRYPOINT` commands will be unable to interprete these values when written in the `exec form` format.

Does NOT work (`exec form`): `ENTRYPOINT ["dotnet", "aspnetapp.dll", "--urls", $URL]`
DOES work (`shell form`): `ENTRYPOINT dotnet aspnetapp.dll --urls $URL`

Refer to: https://stackoverflow.com/questions/37904682/how-do-i-use-docker-environment-variable-in-entrypoint-array

### Docker Compose Gotchas

#### Dynamic Build Args

Wow, just wow.

After hours of trial and error and revisiting the docs and various StackOverflow discussions, it turns out there's really no way to dynamically populate build args from a file.

This is incredibly disappointing, as the only workarounds are either:
1. Passing the build args in manually via the CLI, which of course would be utterly redundant with respect to the `Makefile` protocol I'm already using.
2. Writing some kind of script to generate the `docker-compose.yml` file, which, again, would simply be completely redundant.
3. Hardcoding the values in the `docker-compose.yml` file, which I'm just plain not a fan of.

So I'm pretty much abandoning the Docker compose route here.

For posterity's sake, I'm going to include the `docker-compose.yml` file that I had started on, even though it won't work unless you use one of the three mechanisms I just mentioned for injecting the build args, which I just refuse to do on principle. But feel free to give it a go if you're up for it.

### Make Gotchas

#### Tabs vs Spaces

Makefiles require tabs and will throw an error if you use spaces instead.

For instance, the following will throw an error:

**Makefile**:
```shell
nginx-v2-build:
    cd server/nginx-v2 && docker build --build-arg NGINX_VERSION=$(NGINX_VERSION) --build-arg LUA_JIT_VERSION=$(LUA_JIT_VERSION) --build-arg ENTRYPOINT_VERSION=2 --build-arg NGINX_PORT=$(NGINX_PORT) -t nginx-v2:latest .
```

**Error**:
```shell
(venv) PS C:\Users\Darren\PycharmProjects\Miniprojects\RESTettaStone> make nginx-v2-build
Makefile:43: *** multiple target patterns.  Stop.
```

Whereas this next snippet will work just fine:

```shell
nginx-v2-build:
	cd server/nginx-v2 && docker build --build-arg NGINX_VERSION=$(NGINX_VERSION) --build-arg LUA_JIT_VERSION=$(LUA_JIT_VERSION) --build-arg ENTRYPOINT_VERSION=2 --build-arg NGINX_PORT=$(NGINX_PORT) -t nginx-v2:latest .
```

So Makefiles are picky about this... so what, right?

It's the completely ambiguous error message that makes this particularly frustrating.

The message *"multiple target patterns"* seems to indicate another root cause altogether, so if you don't already associate that error in your mind with a case of using the wrong whitespace for indentation, it can be perplexing indeed.

Just something to keep in mind.

## Language and Framework Gotchas

### Symfony (PHP)

#### Headers vs Body and the Whitespace Issue

I struggled with the fact that I was receiving a response in plain text while I was setting the `Content-Type` header to `application/json`.

I quadruple checked that the string I was sending was perfectly structured to the satisfaction of the JSON specifications, but it just kept showing up as `text/html`.

It turned out that there was some extra whitespace finding its way into the response. The solution to that, as it turns out, is to wrap the entire construction of the response inside of a pair of functions that deal with *output buffering* (`ob_start()` and `ob_end_clean()`).

Here is an example of how that looks:
```php
class UserController extends AbstractController
{
    #[Route('/users', name: 'users')]
    public function hello(EntityManagerInterface $entityManager, SerializerInterface $serializer): Response
    {
        ob_start(); // Start output buffering

        $repository = $entityManager->getRepository(User::class);

        $response = new Response();$users = $repository->findAll();
        $jsonContent = $serializer->serialize($users, 'json');
        $response = new Response($jsonContent, Response::HTTP_OK, ['Content-Type' => 'application/json',]);

        ob_end_clean(); // End output buffering

        return $response;
    }
}
```

#### CLI Race Condition

I encountered a very bizarre peculiarity with running Symfony in a Docker container that I have not encountered in any of the other subprojects.

After a lot of trial and error, it eventually came down to adding a 5 second delay to wait for the application to be fully up and running.

The following `ENTRYPOINT` command resulted in the container exiting immediately:
```shell
ENTRYPOINT symfony serve --port=$PORT
```

Whereas, during my debugging, I was able to get it to work just fine by replacing that line with `ENTRYPOINT bash` and then accessing the internal CLI and entering `symfony serve --port=$PORT`.

Ultimately, the following `ENRTYPOINT` ended up working:
```shell
ENTRYPOINT bash -c "sleep 5 && symfony serve --port=${PORT}"
```

A note on one of the techniques I was able to use to help ensure at least that the application was executing successfully and not throwing any errors was the following command:
```shell
docker run -it symfony_app:1 /bin/bash
```

This super helpful command will likely come in handy in the future when I'm looking to troubleshoot containers that close immediately.

### Rocket (Rust)

#### Pear Codegen Dependency

I encountered a somewhat silly error when trying to build the Rocket app. The message was something to the effect of "failed to run custom build command for `pear_codegen v0.1.5`".

I had tried explicitly specifying a newer version of `pear_codegen` in my `Cargo.toml` configuration file, but it was a dependency of a dependency, so that might be why I was unable to override it.

Interestingly, others have encounters problems with this exact scenario going back about six years: https://github.com/SergioBenitez/Pear/issues/13

The solution that ultimately worked was adding the following command to the `Dockerfile` (even though I explicity define the Rust version to be `nightly` in several places):
```shell
RUN /usr/local/cargo/bin/rustup override set nightly
```

### Django (Python)

#### Project Structure

I'll be honest, I struggled a bit with Django. I had worked with the framework many years ago, so it has been a while. There's that, and then the fact that I have a general preference for working with frameworks that are a lot less rigidly structured. I will always prefer Flask over Django, for example.

In order to use the framework, one is instructed that they must run a couple of code generation steps, and then modify the generated boilerplate as needed. Well, I kind of skipped that step, to make it more like working with Flask (where you build from bottom up). This led to some initial difficulties while I was getting to know exactly how the files are meant to be named and structured throughout.

#### URL Routing

I faced similar challenges with URL routing as I did with the project structure.

This might have been exacerbated by my Docker container networking and Nginx configurations for this particular project. For example, I disabled i18n in the `settings.py` to avoid any `en` prefixes.

#### Django REST Framework

It is pretty clear that Django proper is meant to be server HTML files (in the form of templates). This particular use case here though is REST endpoints exclusively (for the backend portion, at least).

For these reasons, I'd likely go with the official Django REST framework over traditional Django for such use cases.

In fact, I may add such a subproject to the ever-growing collection here.

### Zig

#### Postgres Connection Pool Inconsistency Problem

Full CRUD works, however, there is a problem with it behaving inconsistently and sometimes throwing errors about the database being closed.

I'm leaving this as a problem to solve in the future.

### Fat Free (PHP)

#### Dependency Source

So I had an interesting experience with getting Fat Free to work right. I kept getting 500 errors automatically with every endpoint. It turned out that there was an issue with `E_STRICT` being used but deprecated for the latest versions of PHP. I had the option to downgrade PHP, but didn't really want to go that route.

Another option I had was to patch the code directly in the downloaded package. In fact, I started adding a few `sed` commands to my `Dockerfile` to accomplish just that:
```shell
# Patch the offending line in FatFree framework's base.php
RUN sed -i 's/error_reporting((E_ALL|E_STRICT)/error_reporting(E_ALL/' vendor/bcosca/fatfree-core/base.php
```

And I was using various `grep` calls to find other issues including a nullable parameter thing:
```shell
# grep -Po "function \w+\((.*?)\)" vendor/bcosca/fatfree-core/base.php

# Patch the Preview::render() method to fix implicit nullable parameter
# Patch the functions referencing $hive
RUN sed -i 's/array $hive=NULL/?array $hive=NULL/g' vendor/bcosca/fatfree-core/base.php
```

But I also kept wondering how these simple fixes hadn't already been done by the maintainers of the repo.

Well, it turns out that the changes _had been made_. I searched inside of GitHub for the offending bits of code, and they were all fixed!

So I spend a bit of time just absolutely puzzled as to what was going on. Of course, I double, even triple, checked that the version number specified in my `Dockerfile` and in my `composer.json` file reflected the latest release according to GitHub.

Suddenly, at some point, I noticed that the repository I was searching through on the GitHub.com website was at `f3-factory/fatfree-core`, not `bcosca/fatfree-core`.

I of course remedied that immediately and everything worked!

So the moral of the story here - the actual "Gotcha!" - is to keep an eye out for scenarios where there can be more than one conflicting repository, probably due to a forking or something like that.

## Database Gotchas

### Postgres

#### pg_stat_activity Locks

I encountered my first case of unfinished processes locking my Postgres database.

In order to determine which processes are causing the locks, you can run the following query:
```sql
SELECT * FROM pg_stat_activity;
```

And then to remove the locks, you can run the following query:
```sql
REVOKE CONNECT ON DATABASE postgres FROM PUBLIC, myusername;

SELECT 
    pg_terminate_backend(pid) 
FROM 
    pg_stat_activity 
WHERE 
    -- don't kill my own connection!
    pid <> pg_backend_pid()
    -- don't kill the connections to other databases
    AND datname = 'postgres'
    ;
```

For some additional background:
* https://stackoverflow.com/questions/48783188/postgres-table-queries-not-responding
* https://stackoverflow.com/questions/17654033/how-to-use-pg-stat-activity
* https://stackoverflow.com/questions/5108876/kill-a-postgresql-session-connection

As for what specifically kept causing the locks in the first place, I have a few theories, but I did not test any of them out directly (there were over 20 different locks by the time I noticed they were causing problems).

They did occur when I started adding database interaction during the running of the [full scale CRUD testing suites](#using-python-script-for-more-advanced-testing).

## Comparisons

### Projects by Language

Please note that some of these categorizations are a bit arbitrary and open to debate. I simply wanted to cluster them for readability sake (refer to the concept of ["chunking"](https://en.wikipedia.org/wiki/Chunking_(psychology)) in cognitive psychology research into working memory).

#### Classic

See also: [Performant](#performant)

* **C**: See [Series of Tubes repo](https://github.com/darren277/series-of-tubes).
* **C++**:
  * [Crow](https://github.com/darren277/RESTettaStone/tree/master/backend/crowapp).

#### Performant

See also: [C and C++](#classic)

These are the languages frequently used for compiled system level applications, where memory management and type safety are of critical concern.

* **Rust**:
  * [Actix](https://github.com/darren277/RESTettaStone/tree/master/backend/actixapp).
  * [Rocket](https://github.com/darren277/RESTettaStone/tree/master/backend/rocketapp).
  * See also: [Wasm-FRP repo](https://github.com/darren277/wasm-frp).
  * Yew (Front end framework): See [Wasm-FRP repo](https://github.com/darren277/wasm-frp).
* **Zig**:
  * [Zig](https://github.com/darren277/RESTettaStone/tree/master/backend/zigapp).
* **Go**:
  * [Go](https://github.com/darren277/RESTettaStone/tree/master/backend/goapp).
* **D**:
  * [Vibe](https://github.com/darren277/RESTettaStone/tree/master/backend/vibeapp).

#### Object Oriented

This category is pretty straight forward. These are the primarily object-oriented languages.

(PS: As a personal aside, these are by far my least favorite languages to work with. I frequently refer to them as the boilerplate languages as it takes so many lines of code to write the equivalent to other languages. And it's not even that I dislike object oriented programming. Python, JavaScript, and Ruby also allow for OOP without all the excessive boilerplate. See also: The [discussion regarding language simplicity](#simplicity))

* **C#**:
  * [Asp.Net](https://github.com/darren277/RESTettaStone/tree/master/backend/aspnetapp).
* **Java**:
  * [SpringBoot](https://github.com/darren277/RESTettaStone/tree/master/backend/springbootapp).
  * [Tomcat](https://github.com/darren277/RESTettaStone/tree/master/backend/tomcatapp).

#### Procedural

* **Fortran**:
  * (Still incoming) [Fortran](https://github.com/darren277/RESTettaStone/tree/master/backend/fortranapp).
* **Pascal**:
  * (Still incoming) [Pascal](https://github.com/darren277/RESTettaStone/tree/master/backend/pascalapp).
* **COBOL**:
  * (Still incoming) [COBOL](https://github.com/darren277/RESTettaStone/tree/master/backend/cobolapp).

#### Functional

These languages are the prototypical functional programming languages.

* **Lisp**:
  * (Still incoming) [Lisp](https://github.com/darren277/RESTettaStone/tree/master/backend/lispapp).
* **Haskell**:
  * (Still incoming) [Haskell](https://github.com/darren277/RESTettaStone/tree/master/backend/haskellapp).
  * (Still incoming) [Spock](https://github.com/darren277/RESTettaStone/tree/master/backend/spockapp).

#### Multi-Paradigm

With these languages, you get to have the best of both worlds. You can write object-oriented code, or you can write functional code. Or you can mix the two.

* **Scala**:
  * [Play](https://github.com/darren277/RESTettaStone/tree/master/backend/playapp).
* **Swift**:
  * [Swift](https://github.com/darren277/RESTettaStone/tree/master/backend/swiftapp).
* **F#**:
  * [F#](https://github.com/darren277/RESTettaStone/tree/master/backend/fsharpapp).

#### Versatile or Other

This category is almost a catch all of sorts. These are languages that can be written with different paradigms in mind, perhaps have loose typing, and are generally more flexible. While I created a dedicated section for Scala, Swift, and F# as the "multi-paradigm" languages, they could just as easily fit into this cluster. However, [as mentioned above](#projects-by-language), I am categorizing these mostly for the sake of readability.

Some of these languages are frequently categorized as "scripting languages." I'm a not a big fan of that particular characterization as it gives the impression that they are less capable than the others, when in fact, their versatility makes them that much more powerful. They are all Turing complete languages, so if you find them particularly limiting, that's just a skill issue.

And then there's Prolog. Prolog is a truly unique language as it falls under the rare category of logic programming.

Note that I consider [JavaScript](#javascript) to fall into this category as well, but it gets its own section because there are so many frameworks built for it.

* **Lua**:
  * [Lua](https://github.com/darren277/RESTettaStone/tree/master/backend/luaapp).
* **Perl**:
  * [Perl](https://github.com/darren277/RESTettaStone/tree/master/backend/perlapp).
* **PHP**:
  * [Fat Free](https://github.com/darren277/RESTettaStone/tree/master/backend/fatfreeapp).
  * [Laravel](https://github.com/darren277/RESTettaStone/tree/master/backend/laravelapp).
  * [Symfony](https://github.com/darren277/RESTettaStone/tree/master/backend/symfonyapp).
* **Python**:
  * [Django](https://github.com/darren277/RESTettaStone/tree/master/backend/djangoapp).
  * [Flask](https://github.com/darren277/RESTettaStone/tree/master/backend/flaskapp).
  * (Still incoming) [Chalice](https://github.com/darren277/RESTettaStone/tree/master/other/chaliceapp).
* **Ruby**:
  * [Rails](https://github.com/darren277/RESTettaStone/tree/master/backend/railsapp).
* **Prolog**:
  * [Prolog](https://github.com/darren277/RESTettaStone/tree/master/backend/prologapp).

#### JavaScript

This language gets its own category simply because it is so frequently used in front end frameworks.

* **Back End JS**:
  * [Bun](https://github.com/darren277/RESTettaStone/tree/master/backend/bunapp).
  * [Firebase](https://github.com/darren277/RESTettaStone/tree/master/backend/firebaseapp).
  * [Node](https://github.com/darren277/RESTettaStone/tree/master/backend/nodeapp).
* **Front End JS**:
  * [Angular](https://github.com/darren277/RESTettaStone/tree/master/frontend/angularapp).
  * [Gatsby](https://github.com/darren277/RESTettaStone/tree/master/frontend/gatsbyapp).
  * [Next](https://github.com/darren277/RESTettaStone/tree/master/frontend/nextapp).
  * [React](https://github.com/darren277/RESTettaStone/tree/master/frontend/reactapp).
  * [React Fiber](https://github.com/darren277/RESTettaStone/tree/master/frontend/reactfiberapp).
  * [Vue](https://github.com/darren277/RESTettaStone/tree/master/frontend/vueapp).
  * [Expo / React Native](https://github.com/darren277/RESTettaStone/tree/master/other/expoapp).
  * [Electron](https://github.com/darren277/RESTettaStone/tree/master/other/electronapp).

### Metrics

**NOTE**: **This section is a work in progress.**

This is where I will discuss the various trade offs between the different languages and frameworks.

#### Qualitative

##### Simplicity

This is where I will discuss the trade-off between simplicity and complexity of code (from a human readability standpoint, not in terms of, say, cyclic complexity, which is a more quantitative measure). This includes factors such as intuitiveness, ease of use, and the number of lines of code needed to accomplish a given task.

#### Quantitative

##### Performance

TBD...

#### Both Qualitative and Quantitative

##### Security

TBD...

##### Safety

Type safety, memory safety, etc.

# Optimization

## Latency

TBD...

## Docker Image Size

### Table Sorted Alphabetically

| App | Image Size  |
|-----------|-------------|
|	actix_app	| 	1.88 GB	   |
|	angular_app	| 	1.88 GB	   |
|	aspnet_app	| 	223.67 MB	 |
|	bun_app	| 	226.27 MB	 |
|	clojure_app	| 	623.15 MB	 |
|	cobol_app	| 	243.88 MB	 |
|	crow_app	| 	932.74 MB	 |
|	debugger	| 	11.05 MB	  |
|	django_app	| 	1.07 GB	   |
|	fatfree_app	| 	618.25 MB	 |
|	firebase_app	| 	1.84 GB	   |
|	flask_app	| 1.03 GB	    |
|	fortran_app	| 	508.85 MB	 |
|	fsharp_app	| 	1.12 GB	   |
|	gatsby_app	| 	1.59 GB	   |
|	go_app	| 	852.56 MB	 |
|	grpc_client	| 	1.07 GB	   |
|	grpc_server	| 	1.4 GB	    |
|	haskell_app	| 	8.12 GB	   |
|	laravel_app	| 	707.13 MB	 |
|	lisp_app	| 	1.26 GB	   |
|	lua_app	| 	413.29 MB	 |
|	next_app	| 	888.96 MB	 |
|	nginx	| 	405.53 MB	 |
|	node_app	| 	1.13 GB	   |
|	pascal_app	| 	1.72 GB	   |
|	perl_app	| 	1.86 GB	   |
|	php_app	| 	649.19 MB	 |
|	play_app	| 	288.35 MB	 |
|	postgres	| 	377.36 MB	 |
|	postgres	| 	373.13 MB	 |
|	postgres	| 	212.85 MB	 |
|	prolog_app	| 	577.73 MB	 |
|	rails_app	| 	1.16 GB	   |
|	react_app	| 	541.67 MB	 |
|	reactfiber_app	| 	1.85 GB	   |
|	rocket_app	| 	2.37 GB	   |
|	spock_app	| 	5.83 GB	   |
|	springboot_app	| 	809.8 MB	  |
|	swift_app	| 	345.7 MB	  |
|	symfony_app	| 	4.65 GB	   |
|	tomcat_app	| 	681.22 MB	 |
|	vibe_app	| 	1.49 GB	   |
|	vue_app	| 	577.67 MB	 |
|	zig_app	| 	567.78 MB	 |

### Table Sorted by Image Size

| App | Image Size  |
|-----------|-------------|
|	haskell_app	| 	8.12 GB	   |
|	spock_app	| 	5.83 GB	   |
|	symfony_app	| 	4.65 GB	   |
|	rocket_app	| 	2.37 GB	   |
|	actix_app	| 	1.88 GB	   |
|	angular_app	| 	1.88 GB	   |
|	perl_app	| 	1.86 GB	   |
|	reactfiber_app	| 	1.85 GB	   |
|	firebase_app	| 	1.84 GB	   |
|	pascal_app	| 	1.72 GB	   |
|	gatsby_app	| 	1.59 GB	   |
|	vibe_app	| 	1.49 GB	   |
|	grpc_server	| 	1.4 GB	    |
|	lisp_app	| 	1.26 GB	   |
|	rails_app	| 	1.16 GB	   |
|	node_app	| 	1.13 GB	   |
|	fsharp_app	| 	1.12 GB	   |
|	grpc_client	| 	1.07 GB	   |
|	django_app	| 	1.07 GB	   |
|	flask_app	| 1.03 GB	    |
|	clojure_app	| 	623.15 MB	 |
|	crow_app	| 	932.74 MB	 |
|	next_app	| 	888.96 MB	 |
|	go_app	| 	852.56 MB	 |
|	springboot_app	| 	809.8 MB	  |
|	laravel_app	| 	707.13 MB	 |
|	tomcat_app	| 	681.22 MB	 |
|	php_app	| 	649.19 MB	 |
|	fatfree_app	| 	618.25 MB	 |
|	prolog_app	| 	577.73 MB	 |
|	react_app	| 	541.67 MB	 |
|	vue_app	| 	577.67 MB	 |
|	zig_app	| 	567.78 MB	 |
|	fortran_app	| 	508.85 MB	 |
|	lua_app	| 	413.29 MB	 |
|	nginx	| 	405.53 MB	 |
|	postgres	| 	377.36 MB	 |
|	postgres	| 	373.13 MB	 |
|	swift_app	| 	345.7 MB	  |
|	play_app	| 	288.35 MB	 |
|	cobol_app	| 	243.88 MB	 |
|	bun_app	| 	226.27 MB	 |
|	aspnet_app	| 	223.67 MB	 |
|	postgres	| 	212.85 MB	 |

### Some Potential Solutions

#### Multi-Stage Builds

TBD...
