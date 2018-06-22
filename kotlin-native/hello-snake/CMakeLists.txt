cmake_minimum_required(VERSION 3.8)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/KotlinCMakeModule)

project(hello-snake Kotlin)

konanc_executable(
    NAME hello-snake
    SOURCES src/opengl.kt src/snake.kt)

