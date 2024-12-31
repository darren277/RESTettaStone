<?php
//ob_start();
header('Content-Type: application/json');

$pg = pg_connect("host=".getenv('PG_HOST')." port=".getenv('PG_PORT')." dbname=".getenv('PG_DB')." user=".getenv('PG_USER')." password=".getenv('PG_PASS'));


if (strpos($_SERVER['REQUEST_URI'], '/users/') !== false) {
    $segments = explode('/', trim($_SERVER['REQUEST_URI'], '/'));

    // Ensure the URL contains at least 3 parts (e.g., "app.php/users/1")
    if (count($segments) < 3 || !is_numeric($segments[2])) {
        echo json_encode(['error' => 'Invalid or missing user ID']);
        http_response_code(400); // Bad Request
        exit;
    }

    $id = (int)$segments[2]; // Safely cast to integer

    if ($_SERVER['REQUEST_METHOD'] === 'PUT') {
        $data = json_decode(file_get_contents('php://input'), true);

        $result = pg_query_params($pg, "SELECT * FROM users WHERE id = $1", [$id]);
        if (!$result || pg_num_rows($result) === 0) {
            http_response_code(404);
            echo json_encode(['error' => 'User not found']);
            exit;
        }

        $result = pg_query_params($pg, "UPDATE users SET email = $1 WHERE id = $2", [$data['email'], $id]);

        echo json_encode(['status' => 'success']);
        exit;
    }
    if ($_SERVER['REQUEST_METHOD'] === 'DELETE') {
        $result = pg_query_params($pg, "SELECT * FROM users WHERE id = $1", [$id]);

        if (!$result || pg_num_rows($result) === 0) {
            http_response_code(404);
            echo json_encode(['error' => 'User not found']);
            exit;
        }

        $result = pg_query_params($pg, "DELETE FROM users WHERE id = $1", [$id]);

        echo json_encode(['status' => 'success']);
        exit;
    }
    if ($_SERVER['REQUEST_METHOD'] === 'GET') {
        $result = pg_query_params($pg, "SELECT * FROM users WHERE id = $1", [$id]);
        if (!$result || pg_num_rows($result) === 0) {
            http_response_code(404);
            echo json_encode(['error' => 'User not found']);
            exit;
        }

        // Use pg_fetch_assoc to fetch a single row
        $user = pg_fetch_assoc($result);
        echo json_encode($user);
        exit;
    }
} elseif (strpos($_SERVER['REQUEST_URI'], '/users') !== false) {
    if ($_SERVER['REQUEST_METHOD'] === 'POST') {
        $data = json_decode(file_get_contents('php://input'), true);
        $result = pg_query_params($pg, "INSERT INTO users (email) VALUES ($1)", [$data['email']]);
        echo json_encode(['status' => 'success']);
        exit;
    }

    $result = pg_query($pg, "SELECT * FROM users");
    $users = pg_fetch_all($result);
    echo json_encode($users);
    exit;
}

?>
