module Database
    using LibPQ

    """
        get_connection()

    Create and return a PostgreSQL database connection using environment variables.
    Falls back to default values if environment variables are not set.

    Returns:
        LibPQ.Connection: An active database connection
    """
    function get_connection()
        host = get(ENV, "PG_HOST", "localhost")
        port = get(ENV, "PG_PORT", "5432")
        user = get(ENV, "PG_USER", "postgres")
        pass = get(ENV, "PG_PASS", "")
        db = get(ENV, "PG_DB", "postgres")

        conn_string = "dbname=$db host=$host port=$port user=$user password=$pass"
        return LibPQ.Connection(conn_string)
    end

    """
        query_rows(sql::String, params=nothing)

    Execute an SQL query and return results as an array of dictionaries.

    Arguments:
        sql::String: The SQL query to execute
        params: Optional parameters for the query

    Returns:
        Array: An array of dictionaries, each representing a row
    """
    function query_rows(sql::String, params=nothing)
        conn = get_connection()

        try
            # Execute the query with or without parameters
            result = params === nothing ? execute(conn, sql) : execute(conn, sql, params)

            # Get column names
            columns = propertynames(result)

            # Convert to row-based format
            rows = []
            for row in result
                row_dict = Dict{Symbol, Any}()
                for (i, col) in enumerate(columns)
                    row_dict[Symbol(col)] = row[i]
                end
                push!(rows, row_dict)
            end

            return rows
        finally
            # Make sure connection is closed even if an error occurs
            close(conn)
        end
    end

    """
        query_single_row(sql::String, params=nothing)

    Execute an SQL query and return a single row as a dictionary.

    Arguments:
        sql::String: The SQL query to execute
        params: Optional parameters for the query

    Returns:
        Dict: A dictionary representing a single row, or nothing if no rows found
    """
    function query_single_row(sql::String, params=nothing)
        rows = query_rows(sql, params)
        return isempty(rows) ? nothing : rows[1]
    end

    """
        get_all_users()

    Get all users from the database.

    Returns:
        Array: An array of dictionaries, each representing a user
    """
    function get_all_users()
        return query_rows("SELECT * FROM users ORDER BY id")
    end

    """
        get_user_by_id(id)

    Get a single user by ID.

    Arguments:
        id: The user ID to look up

    Returns:
        Union{Dict,Nothing}: A dictionary representing the user, or nothing if not found
    """
    function get_user_by_id(id)
        return query_single_row("SELECT * FROM users WHERE id = \$1", [id])
    end

    """
        create_user(name, email)

    Create a new user.

    Arguments:
        name: The user's name
        email: The user's email

    Returns:
        Dict: A dictionary representing the created user
    """
    function create_user(name, email)
        result = query_single_row("INSERT INTO users (name, email) VALUES (\$1, \$2) RETURNING *", [name, email])
        return result
    end

    """
        update_user(id, data)

    Update an existing user.

    Arguments:
        id: The user ID to update
        data: Dictionary containing fields to update

    Returns:
        Union{Dict,Nothing}: A dictionary representing the updated user, or nothing if not found
    """
    function update_user(id, data)
        # Build dynamic update SQL
        fields = []
        values = []
        for (key, value) in data
            push!(fields, "$(key) = \$(length(values) + 1)")
            push!(values, value)
        end

        # If no fields to update, return the existing user
        if isempty(fields)
            return get_user_by_id(id)
        end

        # Add the id as the last parameter
        push!(values, id)

        # Construct and execute the query
        sql = "UPDATE users SET $(join(fields, ", ")) WHERE id = \$(length(values)) RETURNING *"
        return query_single_row(sql, values)
    end

    """
        delete_user(id)

    Delete a user by ID.

    Arguments:
        id: The user ID to delete

    Returns:
        Union{Dict,Nothing}: A dictionary representing the deleted user, or nothing if not found
    """
    function delete_user(id)
        return query_single_row("DELETE FROM users WHERE id = \$1 RETURNING *", [id])
    end
end
