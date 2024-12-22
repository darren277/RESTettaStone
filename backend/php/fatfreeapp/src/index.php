<?php

// if (!defined('E_STRICT')) {
//     define('E_STRICT', 0);
// }

error_reporting(E_ALL & ~E_DEPRECATED);

// Composer f3 load style
require_once("./vendor/autoload.php");
$f3 = Base::instance();

$db=new DB\SQL('pgsql:host='.$_ENV['PG_HOST'].';port='.$_ENV['PG_PORT'].';dbname='.$_ENV['PG_DB'], $_ENV['PG_USER'], $_ENV['PG_PASS']);

$f3->route('GET /users',
    function($f3) use ($db) {
        $result = $db->exec('SELECT * FROM users');
        header('Content-Type: application/json');
    	echo json_encode($result);
    }
);

$f3->route('GET /users/@id',
    function($f3) use ($db) {
        $id = $f3->get('PARAMS.id');
        $result = $db->exec('SELECT * FROM users WHERE id = ?', $id);
        if (empty($result)) {
            $f3->error(404);
        }
        header('Content-Type: application/json');
        //$result = array('id'=>$id, 'email'=>'test@email.com');
    	echo json_encode($result[0]);
    }
);

// $f3->route('POST /users',
//     function($f3) use ($db) {
//         $data = json_decode($f3->get('BODY'), true);
//         $result = $db->exec('INSERT INTO users (email) VALUES (?)', $data['email']);
//         header('Content-Type: application/json');
//     	echo json_encode($result);
//     }
// );

$f3->route('POST /users',
    function($f3) use ($db) {
        header('Content-Type: application/json');

        $body = $f3->get('BODY');
        file_put_contents('php://stderr', "Body: $body\n");

        $data = json_decode($body, true);

        // Check for JSON decoding errors
        if (json_last_error() !== JSON_ERROR_NONE) {
            echo json_encode(['error' => 'Invalid JSON: ' . json_last_error_msg()]);
            return;
        }

        // Validate input
        if (empty($data['email'])) {
            echo json_encode(['error' => 'Missing email field']);
            return;
        }

        try {
            // Attempt the database insert
            $result = $db->exec('INSERT INTO users (email) VALUES (?)', $data['email']);

            echo json_encode(['success' => true, 'id' => $db->lastInsertId(), 'email' => $data['email']]);
        } catch (PDOException $e) {
            // Log database errors
            file_put_contents('php://stderr', "Database error: {$e->getMessage()}\n");
            echo json_encode(['error' => 'Database error: ' . $e->getMessage()]);
        }
    }
);

// $f3->route('PUT /users/@id',
//     function($f3) use ($db) {
//         $id = $f3->get('PARAMS.id');
//         $data = json_decode($f3->get('BODY'), true);
//         $result = $db->exec('UPDATE users SET email = ? WHERE id = ?', $data['email'], $id);
//         if (empty($result)) {
//             $f3->error(404);
//         }
//         header('Content-Type: application/json');
//     	echo json_encode($result);
//     }
// );

$f3->route('PUT /users/@id',
    function($f3) use ($db) {
        $id = $f3->get('PARAMS.id');
        $data = json_decode($f3->get('BODY'), true);

        // Ensure both parameters exist
        if (!$id || !isset($data['email'])) {
            $f3->error(400, 'Missing required parameters');
            return;
        }

        //$result = $db->exec('UPDATE users SET email = ? WHERE id = ?', $data['email'], $id);
        // SQLSTATE[08P01]: <<Unknown error>>: 7 ERROR:  bind message supplies 1 parameters, but prepared statement "pdo_stmt_00000001" requires 2 [/var/www/html/vendor/fatfree-core/db/sql.php:230]
        $result = $db->exec('UPDATE users SET email = ? WHERE id = ?', [$data['email'], $id]);

        if (empty($result)) {
            $f3->error(404); // Record not found
            return;
        }

        header('Content-Type: application/json');
        echo json_encode($result);
    }
);

$f3->route('DELETE /users/@id',
    function($f3) use ($db) {
        $id = $f3->get('PARAMS.id');
        $result = $db->exec('DELETE FROM users WHERE id = ?', $id);
        if (empty($result)) {
            $f3->error(404);
        }
        header('Content-Type: application/json');
    	//echo json_encode($result);
    	echo json_encode(array('id'=>$id));
    }
);

$f3->run();
