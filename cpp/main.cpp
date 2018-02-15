#include <iostream>
#include <boost/coroutine/all.hpp>

typedef boost::coroutines::asymmetric_coroutine<int>::pull_type pull_type;
typedef boost::coroutines::asymmetric_coroutine<int>::push_type push_type;

void fn1(push_type &sink) {
    for (int i = 0; i < 10; ++i) {
        sink(i);
    }
}

int main() {
    std::cout << "Hello" << std::endl;

    pull_type source(fn1);
    while (source) {
        int n = source.get();
        std::cout << "n = " << n << std::endl;
        source();
    }

    return 0;
}