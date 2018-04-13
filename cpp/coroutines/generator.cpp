#include <iostream>
//#include <future>
#include <experimental/coroutine>
#include <experimental/generator>
//#include <experimental/future>


std::experimental::generator<int> generatorForNumbers(int begin, int inc = 1) {
    for (int i = begin;; i += inc) {
        co_yield i;
    }
}

int main() {
}