<?php
$pg = pg_connect("host=".getenv('PG_HOST')." port=".getenv('PG_PORT')." dbname=".getenv('PG_DB')." user=".getenv('PG_USER')." password=".getenv('PG_PASS'));

// Note: Obviously this is kind of an awkward URL (i.e. `app.php/users`), so it will have to be refined in the future.
if (strpos($_SERVER['REQUEST_URI'], '/users') !== false) {
    header('Content-Type: application/json');
    $result = pg_query($pg, "SELECT * FROM users");
    $users = pg_fetch_all($result);
    echo json_encode($users);
}
?>
