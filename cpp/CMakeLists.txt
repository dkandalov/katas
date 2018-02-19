cmake_minimum_required(VERSION 3.9)

set(CMAKE_CXX_STANDARD 17)

set(GHSMULTI 1)
set(Boost_USE_STATIC_LIBS 1)
set(CMAKE_FIND_LIBRARY_PREFIXES "")
set(CMAKE_FIND_LIBRARY_SUFFIXES "")
set(BOOST_ROOT /usr/local/Cellar/boost@1.60/1.60.0)
set(Boost_INCLUDE_DIRS /usr/local/Cellar/boost@1.60/1.60.0/include)
set(BOOST_LIBRARYDIR /usr/local/Cellar/boost@1.60/1.60.0/lib)
find_package(Boost 1.60.0 COMPONENTS coroutine context thread-mt REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})

project(cpp)

add_executable(cpp main.cpp 1.h 2.h)
target_link_libraries(cpp ${Boost_LIBRARIES})