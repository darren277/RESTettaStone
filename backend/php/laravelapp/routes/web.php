<?php

use Illuminate\Support\Facades\Route;

use App\Models\User;


// Web Routes: Here is where you can register web routes for your application. These routes are loaded by the RouteServiceProvider within a group which contains the "web" middleware group. Now create something great!
Route::get('/', function () {return view('welcome');});


Route::get('/users', function () {
    //return response()->json(['name' => 'Abigail', 'state' => 'CA']);
    return response()->json(User::all());
});

// GET /users/<id>
Route::get('/users/{id}', function ($id) {
    $user = User::find($id);

    if (!$user) {
        return response()->json(['error' => 'User not found'], 404);
    }

    return response()->json($user);
});

// POST /users
Route::post('/users', function () {
    $data = request()->only(['id', 'email']);
    $user = User::create($data);
    return response()->json($user);
});

// PUT /users/<id>
Route::put('/users/{id}', function ($id) {
    $user = User::find($id);

    if (!$user) {
        return response()->json(['error' => 'User not found'], 404);
    }

    $data = request()->only(['email']);
    $user->fill($data)->save();
    return response()->json($user);
});

// DELETE /users/<id>
Route::delete('/users/{id}', function ($id) {
    $user = User::find($id);

    if (!$user) {
        return response()->json(['error' => 'User not found'], 404);
    }

    $user->delete();
    return response()->json(['message' => 'User deleted']);
});


// $user = User::create(['email' => 'laravel@test.com']);

// foreach (Flight::all() as $flight) {echo $flight->name;}

// $flights = Flight::where('active', 1)->orderBy('name')->take(10)->get();


