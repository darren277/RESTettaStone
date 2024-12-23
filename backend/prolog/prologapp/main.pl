:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

:- use_module(library(odbc)).

:- http_handler(root(user_count), db_query_handler, []).
:- http_handler(/, default_handler, []).

:- http_handler(root(users/UserID), user_handler, []).
:- http_handler(root(users), users_handler, []).

users_handler(Request) :-
    % Dispatch based on the method
    memberchk(method(Method), Request),
    (   Method = get -> list_users_handler(Request)
    ;   Method = post -> create_user_handler(Request)
    ;   reply_json_dict(_{error: 'Method not allowed'}, [status(405)])
    ).

user_handler(Request) :-
    safe_log('Received Request: ':Request),

    % Extract the UserID once from the path
    (   extract_user_id_from_path(Request, UserID)
    ->  safe_log('Extracted UserID: ':UserID),
        handle_method(Request, UserID)
    ;   reply_json_dict(_{error: 'Invalid UserID in path'}, [status(400)]), fail
    ).

handle_method(Request, UserID) :-
    memberchk(method(Method), Request),
    (   Method = get -> user_by_id_handler(UserID)
    ;   Method = put -> update_user_handler(UserID, Request)
    ;   Method = delete -> delete_user_handler(UserID, Request)
    ;   reply_json_dict(_{error: 'Method not allowed'}, [status(405)])
    ).

default_handler(Request) :-
    writeln('Unhandled request: ':Request),
    reply_json_dict(_{error: 'Unhandled request'}).

server(Port) :- http_server(http_dispatch, [port(Port)]).

fetch_users(Users) :-
    safe_log('Connecting to the database'),
    odbc_connect('postgres', Connection, []),
    safe_log('Fetching users'),
    findall(_{id: ID, email: Email},
            (   odbc_query(Connection, 'SELECT id, email FROM users', row(ID, Email))
            ->  true
            ;   writeln('No rows found')
            ),
            Users),
    safe_log('Users fetched: ':Users),
    odbc_disconnect(Connection).

get_user_by_id(UserID, User) :-
    safe_log('UserID type check: '),
    (   integer(UserID)
    ->  safe_log('UserID is a valid integer: ':UserID)
    ;   safe_log('UserID is invalid: ':UserID), fail
    ),

    safe_log('Fetching user with ID: ':UserID),
    odbc_connect('postgres', Connection, []),
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
    odbc_prepare(Connection, 'INSERT INTO users (email) VALUES (?)', [varchar], Statement),
    odbc_execute(Statement, [Email]),
    odbc_free_statement(Statement),
    odbc_disconnect(Connection).

update_user(UserID, User) :-
    odbc_connect('postgres', Connection, []),
    odbc_prepare(Connection, 'UPDATE users SET email = ? WHERE id = ?', [varchar, integer], Statement),
    odbc_execute(Statement, [User.email, UserID], Result),
    odbc_free_statement(Statement),
    odbc_disconnect(Connection),
    safe_log('Result: ':Result),
    (   Result = affected(RowsAffected)
    ->  safe_log('Rows affected: ':RowsAffected),
        (   RowsAffected > 0
        ->  true % Success case
        % ;   throw(http_reply(not_found)) % No rows deleted; throw 404
        ; fail
        )
    ;   throw(http_reply(server_error)) % Unexpected result
    ).

delete_user(UserID) :-
    odbc_connect('postgres', Connection, []),
    odbc_prepare(Connection, 'DELETE FROM users WHERE id = ?', [integer], Statement),
    odbc_execute(Statement, [UserID], Result),
    odbc_free_statement(Statement),
    odbc_disconnect(Connection),
    safe_log('Result: ':Result),
    (   Result = affected(RowsAffected)
    ->  safe_log('Rows affected: ':RowsAffected),
        (   RowsAffected > 0
        ->  true % Success case
        % ;   throw(http_reply(not_found)) % No rows deleted; throw 404
        ; fail
        )
    ;   throw(http_reply(server_error)) % Unexpected result
    ).

list_users_handler(Request) :-
    safe_log('Handling GET /users request'),
    fetch_users(Users),
    reply_json_dict(Users).
    % safe_log('Request received: ':Request),
    % reply_json_dict(_{status: 'debugging'}).

user_by_id_handler(UserID) :-
    catch(
        (
            % Attempt to fetch the user by ID
            (   get_user_by_id(UserID, User)
            ->  % If the user exists, return their data as JSON
                reply_json_dict(User)
            ;   % If no user is found, return a 404 error
                reply_json_dict(_{error: "User not found"}, [status(404)])
            )
        ),
        Error,
        (
            safe_log('Error: ':Error),
            reply_json_dict(_{error: "Something went wrong"}, [status(500)])
        )
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

extract_user_id_from_path(Request, UserID) :-
    % Extract the path from the request
    memberchk(path(Path), Request),
    % Ensure the path starts with '/users/'
    sub_atom(Path, 0, _, _, '/users/'),
    % Extract the part after '/users/'
    sub_atom(Path, 7, _, 0, AtomID),
    % Debug the extracted part
    safe_log('Extracted AtomID: ':AtomID),
    % Convert to a number
    atom_number(AtomID, UserID),
    % Debug the final UserID
    safe_log('Converted UserID: ':UserID).

update_user_handler(UserID, Request) :-
    http_read_json_dict(Request, UserData),
    (   update_user(UserID, UserData)
    ->  reply_json_dict(UserData)
    ;   reply_json_dict(_{error: "User not found"}, [status(404)])
    ).

delete_user_handler(UserID, Request) :-
    (   delete_user(UserID)
    ->  reply_json_dict(_{message: "User deleted"})
    ;   reply_json_dict(_{error: "User not found"}, [status(404)])
    ).
