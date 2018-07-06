cmake_minimum_required(VERSION 3.8)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/KotlinCMakeModule)

project(hello-snake Kotlin)

konanc_library(
    NAME snake-lib
    SOURCES src/opengl.kt src/snake.kt)

konanc_executable(
    NAME snake-main
    SOURCES main/main.kt
    LIBRARIES snake-lib)

konanc_executable(
    NAME snake-test
    SOURCES test/test.kt
    LIBRARIES snake-lib)

