<?php
// Composer f3 load style
require_once("./vendor/autoload.php");
$f3 = Base::instance();

$f3->route('GET /',
    function() {
        echo 'Hello, world!!';
    }
);

$db=new DB\SQL('pgsql:host='.$_ENV['PG_HOST'].';port='.$_ENV['PG_PORT'].';dbname='.$_ENV['PG_DB'], $_ENV['PG_USER'], $_ENV['PG_PASS']);

$f3->route('GET /users',
    function($f3) use ($db) {
        $result = $db->exec('SELECT * FROM users');
        header('Content-Type: application/json');
        //$id = $f3->get('PARAMS.id');
        //$result = array('id'=>$id, 'name'=>'Taras', 'lastname'=>'Shevchenko');
    	echo json_encode($result);
    }
);

$f3->run();
