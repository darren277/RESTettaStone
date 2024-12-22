:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

:- use_module(library(odbc)).

:- http_handler(root(user_count), db_query_handler, []).
:- http_handler(root(users), list_users_handler, []).
:- http_handler(root(users/UserID), user_by_id_handler, [id(user_by_id), method(get)]).
:- http_handler(root(users/UserID), update_user_handler, [id(update_user), method(put)]).
:- http_handler(root(users/UserID), delete_user_handler, [id(delete_user), method(delete)]).
:- http_handler(root(users), create_user_handler, [method(post)]).

:- http_handler(/, default_handler, []).

default_handler(Request) :-
    writeln('Unhandled request: ':Request),
    reply_json_dict(_{error: 'Unhandled request'}).

server(Port) :- http_server(http_dispatch, [port(Port)]).

fetch_users(Users) :-
    odbc_connect('postgres', Connection, []),
    findall(_{id: ID, email: Email},
            odbc_query(Connection, 'SELECT id, email FROM users', row(ID, Email)), Users),
    odbc_disconnect(Connection).

get_user_by_id(UserID, User) :-
    format('Fetching user with ID: ~w~n', [UserID]),
    odbc_connect('postgres', Connection, []),
    % odbc_query(Connection, 'SELECT id, email FROM users WHERE id = ?', [UserID], [ID, Email])
    odbc_prepare(Connection, 'SELECT id, email FROM users WHERE id = ?', [integer], Statement),
    odbc_execute(Statement, [UserID], row(ID, Email)),
    odbc_free_statement(Statement),
    (   ID = UserID
    ->  User = _{id: ID, email: Email}
    ;   User = null
    ),
    safe_log('User: ':User),
    safe_log('ID: ':ID),
    safe_log('Email: ':Email),
    odbc_disconnect(Connection).

create_user(Data) :-
    Email = Data.get(email),
    safe_log('Creating user with email: ':Email),
    odbc_connect('postgres', Connection, []),
    % odbc_query(Connection, 'INSERT INTO users (email) VALUES (CAST(? AS VARCHAR))', [Email]),
    odbc_prepare(Connection, 'INSERT INTO users (email) VALUES (?)', [varchar], Statement),
    odbc_execute(Statement, [Email]),
    odbc_free_statement(Statement),
    odbc_disconnect(Connection).
    % reply_json_dict(_{status: "success", email: Email}).

update_user(UserID, User) :-
    odbc_connect('postgres', Connection, []),
    odbc_query(Connection, 'UPDATE users SET email = ? WHERE id = ?', [User.email, UserID]),
    odbc_disconnect(Connection).

delete_user(UserID) :-
    odbc_connect('postgres', Connection, []),
    odbc_query(Connection, 'DELETE FROM users WHERE id = ?', [UserID]),
    odbc_disconnect(Connection).

list_users_handler(_Request) :-
    fetch_users(Users),
    reply_json_dict(Users).

user_by_id_handler(Request) :-
    % hardcode the user ID for now
    UserID = 1,
    % extract_user_id_from_path(Request),

    % write('Extracted UserID: '), write_canonical(UserID), nl,
    safe_log('Fetching user by ID: ':UserID),
    safe_log('Request: ':Request),
    format('Fetching user by ID: ~w~n', [UserID]),

    catch(
        get_user_by_id(UserID, User),
        Error,
        (
            safe_log('Error: ':Error),
            throw(http_reply(not_found(UserID)))
        )
    ),
    (   User = null
    ->  % If the user is not found, send a 404 response:
        throw(http_reply(not_found(UserID)))
    ;   % Otherwise, send the user data as JSON:
        % reply_json_dict(User)
        % format('Content-type: application/json~n~n'),
        % json_write(current_output, User).
    ).

create_user_handler(Request) :-
    catch(
        (
            safe_log('Handling POST /users request'),
            safe_log('Request: ':Request),

            http_read_json_dict(Request, Data),
            safe_log('Received data: ':Data),
            (   create_user(Data)
            ->  reply_json_dict(_{status: 'success'})
            ;   reply_json_dict(_{error: 'Failed to create user'}, [status(500)])
            )
        ),
        Error,
        (
            safe_log('Error: ':Error),
            reply_json_dict(_{error: 'Invalid data'}, [status(400)])
        )
    ).

safe_log(Message) :-
    % catch(writeln(Message), _, true).
    format(user_error, '~w~n', [Message]).

extract_user_id_from_path(Request) :-
    % % Extract the user ID from the path
    catch(
        (
            % http_parameters(Request, [id(UserID, [integer])]),
            member(path(Path), Request),
            atom_concat('/users/', AtomID, Path),
            atom_number(AtomID, UserID),  % Convert AtomID to a number
            safe_log('User ID: ':UserID)
        ),
        Error,
        (
            safe_log('Error: ':Error),
            throw(http_reply(not_found))
        )
    ).

update_user_handler(Request) :-
    http_parameters(Request, [id(UserID, [integer])]),
    http_read_json_dict(Request, UserData),
    (   update_user(UserID, UserData)
    ->  reply_json_dict(UserData)
    ;   throw(http_reply(not_found))
    ).

delete_user_handler(Request) :-
    http_parameters(Request, [id(UserID, [integer])]),
    (   delete_user(UserID)
    ->  reply_json_dict(_{message: "User deleted"})
    ;   throw(http_reply(not_found))
    ).
