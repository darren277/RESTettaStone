
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

## Current Status (Tables)

This table represents the status of the project as of November 2022.

I'm leaving the Python options (Flask and Django) until last as they will be by far the easiest to implement for me.

I break down the barriers I've faced into a few fundamental categories:
* **Language**: A lack of experience with the language and some of its specific nuances. This barrier is especially prevalent with languages that use non-object-oriented paradigms (for example, Lisp, Haskell, Prolog, etc).
* **Libraries**: Since I'm using Postgres for the universal database that all the backends are accessing, I need an existing library or implementation for accessing a Postgres database. I think the only place where this was a total blocker was Bun.js. I started looking into the feasibility of writing my own super simple implementation, but abandoned that due to a lack of available time. It turns out, after a couple years have passed, that there is now Postgres support for Bun.js.
* **Networking**: This is a case where I have difficulties accessing the endpoint for some reason. I recall a particular scenario where `0.0.0.0` was not doing what it was supposed to be doing, and I just hit a wall with it and gave up at the time. Looking at the table, it looks like that was one of the Rust frameworks. I plan to revisit that and troubleshoot it further.
* **Build**: This is where the Dockerfile fails to execute every step and the application simply does not run. It looks like Zig was the only such scenario. This is another one I will try revisiting after all this time.

### Backend

|                   | Dockerized | REST Responses | Postgres Interaction | Full CRUD | Current Barrier |
|-------------------|------------|----------------|----------------------|-----------|-----------------|
| Actix (Rust)      | Y          |                |                      |           | Language        |
| Asp.Net (C#)      | Y          | Y              | Y                    |           |                 |
| Bun (JS)          | Y          | Y              |                      |           | Libraries       |
| Cobol             | Y*         | Y              |                      |           | Language        |
| Crow (C++)        | Y          | Y              | Y                    |           |                 |
| Django (Python)   |            |                |                      |           |                 |
| Firebase (JS)     | Y          | Y              | N/A                  |           |                 |
| Flask (Python)    |            |                |                      |           |                 |
| Fortran           |            |                |                      |           | Language        |
| F#                | Y          |                |                      |           | Language        |
| Go                | Y          | Y              | Maybe?               |           |                 |
| Haskell           |            |                |                      |           |                 |
| Laravel (PHP)     | Y          | Y              | Y                    |           |                 |
| Lisp              |            |                |                      |           |                 |
| Lua / OpenResty   |            |                |                      |           |                 |
| Node (JS)         | Y          | Y              | Y                    |           |                 |
| Pascal            | Y          | Y              | Y                    |           |                 |
| Perl              | Y          | Y              |                      |           |                 |
| Play (Scala)      |            |                |                      |           |                 |
| Prolog            | Y          | Y              |                      |           | Language        |
| Rails (Ruby)      | Y          | Y              | Y                    |           |                 |
| Rocket (Rust)     | Y*         | Y              |                      |           | Networking      |
| SpringBoot (Java) | Y          | Y              | Y                    |           |                 |
| Swift             |            |                |                      |           |                 |
| Symfony (PHP)     |            |                |                      |           |                 |
| Vibe (D)          | Y*         | Y              |                      |           | Networking      |
| Zig               |            |                |                      |           | Build           |

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
