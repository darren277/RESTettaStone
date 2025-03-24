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
                for col in columns
                    row_dict[col] = row[col]
                end
                push!(rows, row_dict)
            end

            return rows
        finally
            # Make sure connection is closed even if an error occurs
            close(conn)
        end
    end
end
