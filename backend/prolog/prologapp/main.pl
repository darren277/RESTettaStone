:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).

:- use_module(library(odbc)).

:- http_handler(root(user_count), db_query_handler, []).
:- http_handler(root(users), list_users_handler, []).

server(Port) :- http_server(http_dispatch, [port(Port)]).

fetch_users(Users) :-
    odbc_connect('postgres', Connection, []),
    findall(_{id: ID, email: Email},
            odbc_query(Connection, 'SELECT id, email FROM users', row(ID, Email)), Users),
    odbc_disconnect(Connection).

list_users_handler(_Request) :-
    fetch_users(Users),
    reply_json_dict(Users).
