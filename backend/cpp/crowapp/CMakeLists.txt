cmake_minimum_required(VERSION 3.8)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -lpqxx -lpq")
set(PQXX /usr/local/include/pqxx)

SET(PROJ_SOURCES "")
SET(PROJ_HEADERS "")

project(project)

include(/vcpkg/scripts/buildsystems/vcpkg.cmake)

find_package(Crow CONFIG REQUIRED)

find_library(PQXX_LIB pqxx)
find_library(PQ_LIB pq)

add_executable(
    app main.cpp pq_conn_pool.cpp pq_conn_pool.h
)

target_link_libraries(app PUBLIC Crow::Crow)
target_link_libraries(app PUBLIC -lpqxx -lpq)
