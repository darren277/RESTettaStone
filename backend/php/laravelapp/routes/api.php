<?php

use Illuminate\Http\Request;
use Illuminate\Support\Facades\Route;

// API Routes: Here is where you can register API routes for your application. These routes are loaded by the RouteServiceProvider within a group which is assigned the "api" middleware group. Enjoy building your API!
//Route::middleware('auth:sanctum')->get('/user', function (Request $request) {return $request->user();});

// handle GET and POST requests
Route::middleware('auth:sanctum')->get('/users', function (Request $request) {return $request->user();});
Route::middleware('auth:sanctum')->post('/users', function (Request $request) {return $request->user();});

// handle /users/{id} GET request
Route::middleware('auth:sanctum')->get('/users/{id}', function (Request $request, $id) {return $request->user();});

// handle /users/{id} PUT request
Route::middleware('auth:sanctum')->put('/users/{id}', function (Request $request, $id) {return $request->user();});

// handle /users/{id} DELETE request
Route::middleware('auth:sanctum')->delete('/users/{id}', function (Request $request, $id) {return $request->user();});
