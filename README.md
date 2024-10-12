
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

|                   | Dockerized | REST Responses | Postgres Interaction | Full CRUD | Current Barrier |
|-------------------|------------|----------------|----------------------|-----------|-----------------|
| Actix (Rust)      | Y          | Y              | Y                    |           | ~~Language~~    |
| Asp.Net (C#)      | Y          | Y              | Y                    |           |                 |
| Bun (JS)          | Y          | Y              | Y                    |           | ~~Libraries~~   |
| Cobol             | '*         | '              |                      |           | Language        |
| Crow (C++)        | Y          | Y              | Y                    |           |                 |
| Django (Python)   | '          | '              | '                    |           |                 |
| Firebase (JS)     | '          | '              | N/A                  |           |                 |
| Flask (Python)    | '          | '              | '                    |           |                 |
| Fortran           |            |                |                      |           | Language        |
| F#                | Y          | Y              | Y                    |           | ~~Language~~    |
| Go                | Y          | Y              | Y                    |           |                 |
| Haskell           |            |                |                      |           | Language        |
| Laravel (PHP)     | '          | '              | '                    |           |                 |
| Lisp              |            |                |                      |           | Langauge        |
| Lua / OpenResty   |            |                |                      |           |                 |
| Node (JS)         | '          | '              | '                    |           |                 |
| Pascal            | '          | '              | '                    |           |                 |
| Perl              | Y          | Y              | Y                    |           |                 |
| Play (Scala)      |            |                |                      |           |                 |
| Prolog            | Y          | Y              | Y                    |           | ~~Language~~    |
| Rails (Ruby)      | Y          | Y              | Y                    |           |                 |
| Rocket (Rust)     | '*         | '              |                      |           | Networking      |
| SpringBoot (Java) | Y          | Y              | Y                    |           |                 |
| Swift             | Y          | Y              | Y                    |           |                 |
| Symfony (PHP)     | Y          | Y              | Y                    |           |                 |
| Vibe (D)          | Y          | Y              | Y                    |           | ~~Networking~~  |
| Zig               | Y          | Y              | Y                    |           | ~~Build~~       |

### Frontend

|             | Dockerized | Backend Integration   | Full CRUD |
|-------------|------------|-----------------------|-----------|
| Angular     |            |                       |           |
| Gatsby      |            |                       |           |
| Next        |            |                       |           |
| React       |            |                       |           |
| React Fiber |            |                       |           |
| Vue         |            |                       |           |

## Standing on the Shoulders of Giants

In many cases, I made use of some existing projects out there as templates or boilerplate. I wound up modifying many of the Dockerfiles I found, for example, quite substantially, to suit my custom needs. There were also a few repositories I found on GitHub that helped guide my initial efforts for some of the languages where I significantly lacked direct experience.

One major example of this was the [COBOL backend](https://github.com/azac/cobol-on-wheelchair). I'd have had no idea where to even begin for something like that (well, other than the language specifications and documentation, I suppose - but that would have been unecessarily time consuming, and why reinvent the wheel?).

## Future

The first immediate steps to carry out will be gradually adding each subdirectory as I test them and maybe clean them up a bit by removing unecessary comments and so on.

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

# Lessons

## Gotchas

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

### Language and Framework Gotchas

#### Symfony (PHP)

##### Headers vs Body and the Whitespace Issue

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

##### CLI Race Condition

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

#### Rocket (Rust)

##### Pear Codegen Dependency

I encountered a somewhat silly error when trying to build the Rocket app. The message was something to the effect of "failed to run custom build command for `pear_codegen v0.1.5`".

I had tried explicitly specifying a newer version of `pear_codegen` in my `Cargo.toml` configuration file, but it was a dependency of a dependency, so that might be why I was unable to override it.

Interestingly, others have encounters problems with this exact scenario going back about six years: https://github.com/SergioBenitez/Pear/issues/13

The solution that ultimately worked was adding the following command to the `Dockerfile` (even though I explicity define the Rust version to be `nightly` in several places):
```shell
RUN /usr/local/cargo/bin/rustup override set nightly
```
