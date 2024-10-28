<?php

use Illuminate\Support\Facades\Route;

use App\Models\User;


// Web Routes: Here is where you can register web routes for your application. These routes are loaded by the RouteServiceProvider within a group which contains the "web" middleware group. Now create something great!
Route::get('/', function () {return view('welcome');});


Route::get('/users', function () {
    //return response()->json(['name' => 'Abigail', 'state' => 'CA']);
    return response()->json(['users' => User::all()]);
});


// $user = User::create(['email' => 'laravel@test.com']);

// foreach (Flight::all() as $flight) {echo $flight->name;}

// $flights = Flight::where('active', 1)->orderBy('name')->take(10)->get();


