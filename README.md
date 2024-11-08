
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
  * [Debugger / Tester](#debugger---tester)
  * [Performance Testing with Locust](#performance-testing-with-locust)
- [Lessons](#lessons)
  * [DevOps Gotchas](#devops-gotchas)
    + [Nginx Gotchas](#nginx-gotchas)
      - [Environment Variables](#environment-variables)
    + [Docker Gotchas](#docker-gotchas)
      - [ARGS and Multi Stage Builds](#args-and-multi-stage-builds)
      - [Exec Form vs Shell Form for ENTRYPOINT:](#exec-form-vs-shell-form-for-entrypoint-)
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

|                                                                                                   | Dockerized | REST Responses | Postgres Interaction | Full CRUD | Current Barrier |
|---------------------------------------------------------------------------------------------------|------------|----------------|----------------------|-----------|-----------------|
| [Actix (Rust)](https://github.com/darren277/RESTettaStone/tree/master/backend/actixapp)           | Y          | Y              | Y                    |           | ~~Language~~    |
| [Asp.Net (C#)](https://github.com/darren277/RESTettaStone/tree/master/backend/aspnetapp)          | Y          | Y              | Y                    |           |                 |
| [Bun (JS)](https://github.com/darren277/RESTettaStone/tree/master/backend/bunapp)                 | Y          | Y              | Y                    |           | ~~Libraries~~   |
| Cobol                                                                                             | '*         | '              |                      |           | Language        |
| [Crow (C++)](https://github.com/darren277/RESTettaStone/tree/master/backend/crowapp)              | Y          | Y              | Y                    |           |                 |
| [Django (Python)](https://github.com/darren277/RESTettaStone/tree/master/backend/djangoapp)       | Y          | Y              | Y                    |           |                 |
| [Fat Free (PHP)](https://github.com/darren277/RESTettaStone/tree/master/backend/fatfreeapp)       | Y          | Y              | Y                    |           |                 |
| [Firebase (JS)](https://github.com/darren277/RESTettaStone/tree/master/backend/firebaseapp)       | Y          | Y              | N/A                  |           |                 |
| [Flask (Python)](https://github.com/darren277/RESTettaStone/tree/master/backend/flaskapp)         | Y          | Y              | Y                    |           |                 |
| Fortran                                                                                           |            |                |                      |           | Language        |
| [F#](https://github.com/darren277/RESTettaStone/tree/master/backend/fsharpapp)                    | Y          | Y              | Y                    |           | ~~Language~~    |
| [Go](https://github.com/darren277/RESTettaStone/tree/master/backend/goapp)                        | Y          | Y              | Y                    |           |                 |
| Haskell                                                                                           |            |                |                      |           | Language        |
| [Laravel (PHP)](https://github.com/darren277/RESTettaStone/tree/master/backend/laravelapp)        | Y          | Y              | Y                    |           |                 |
| Lisp                                                                                              |            |                |                      |           | Language        |
| [Lua / OpenResty](https://github.com/darren277/RESTettaStone/tree/master/backend/luaapp)          | Y          | Y              | Y                    |           |                 |
| [Node (JS)](https://github.com/darren277/RESTettaStone/tree/master/backend/nodeapp)               | Y          | Y              | Y                    |           |                 |
| Pascal                                                                                            | '          | '              | '                    |           |                 |
| [Perl](https://github.com/darren277/RESTettaStone/tree/master/backend/perlapp)                    | Y          | Y              | Y                    |           |                 |
| [Play (Scala)](https://github.com/darren277/RESTettaStone/tree/master/backend/playapp)            | Y          | Y              | Y                    |           |                 |
| [Prolog](https://github.com/darren277/RESTettaStone/tree/master/backend/prologapp)                | Y          | Y              | Y                    |           | ~~Language~~    |
| [Rails (Ruby)](https://github.com/darren277/RESTettaStone/tree/master/backend/railsapp)           | Y          | Y              | Y                    |           |                 |
| [Rocket (Rust)](https://github.com/darren277/RESTettaStone/tree/master/backend/rocketapp)         | Y          | Y              | Y                    |           | ~~Networking~~  |
| [SpringBoot (Java)](https://github.com/darren277/RESTettaStone/tree/master/backend/springbootapp) | Y          | Y              | Y                    |           |                 |
| [Swift](https://github.com/darren277/RESTettaStone/tree/master/backend/swiftapp)                  | Y          | Y              | Y                    |           |                 |
| [Symfony (PHP)](https://github.com/darren277/RESTettaStone/tree/master/backend/symfonyapp)        | Y          | Y              | Y                    |           |                 |
| [Tomcat (Java)](https://github.com/darren277/RESTettaStone/tree/master/backend/tomcatapp)         | Y          | Y              | Y                    |           |                 |
| [Vibe (D)](https://github.com/darren277/RESTettaStone/tree/master/backend/vibeapp)                | Y          | Y              | Y                    |           | ~~Networking~~  |
| [Zig](https://github.com/darren277/RESTettaStone/tree/master/backend/zigapp)                      | Y          | Y              | Y                    |           | ~~Build~~       |

### Frontend

|                                                                                              | Dockerized | Backend Integration | Full CRUD |
|----------------------------------------------------------------------------------------------|------------|---------------------|-----------|
| [Angular](https://github.com/darren277/RESTettaStone/tree/master/frontend/angularapp)        | Y          | Y                   |           |
| [Gatsby](https://github.com/darren277/RESTettaStone/tree/master/frontend/gatsbyapp)          | Y          | Y                   |           |
| [Next](https://github.com/darren277/RESTettaStone/tree/master/frontend/nextapp)       | Y          | Y                   |           |
| [React](https://github.com/darren277/RESTettaStone/tree/master/frontend/reactapp)            | Y          | Y                   |           |
| [React Fiber](https://github.com/darren277/RESTettaStone/tree/master/frontend/reactfiberapp) | Y          | Y                   |           |
| [Vue](https://github.com/darren277/RESTettaStone/tree/master/frontend/vueapp)                | Y          | Y                   |           |

### Other

|                                                                                             | Dockerized | Backend Integration | Full CRUD |
|---------------------------------------------------------------------------------------------|------------|---------------------|-----------|
| [Electron](https://github.com/darren277/RESTettaStone/tree/master/other/electronapp)        | N/A        | Y                   |           |
| [Expo / React Native](https://github.com/darren277/RESTettaStone/tree/master/other/expoapp) | N/A        | Y                   |           |
| Chalice                                                                                     |            |                     |           |

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

The Makefile, which requires installation of `make` if you do not already have it, is provided to fascilitate the execution of many of the commands needed to run the different pieces of this project.

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

## Debugger / Tester

I created a Dockerfile for a new container that can be used for testing endpoints.

I've also included one example usage:
```shell
crowapp-test:
	docker exec -it debugger curl http://$(CROWAPP_IP):$(CROWAPP_PORT)/api/users
```

The next thing I'd like to add to this generalized debugger container is a logging system that can create logs either on the host machine or at least be very easily accessible via another `Make` command.

Or, better yet, perhaps create some kind of visual user interface for viewing these logs (or leveraging something like `Kibana` or `Grafana`).

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

In fact, I may add such a subproject to the ever growing collection here.
