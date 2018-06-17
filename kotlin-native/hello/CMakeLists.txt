cmake_minimum_required(VERSION 3.8)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/KotlinCMakeModule)

project(hello Kotlin)

cinterop(
    NAME stdio
    DEF_FILE src/c_interop/stdio.def)

konanc_executable(
    NAME hello
    SOURCES src/hello.kt
    LIBRARIES stdio)

