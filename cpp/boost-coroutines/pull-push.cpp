#include <iostream>
#include <boost/coroutine2/all.hpp>

typedef boost::coroutines2::coroutine<int>::pull_type pull_type;
typedef boost::coroutines2::coroutine<int>::push_type push_type;

void log(int n) {
    std::cout << "n = " << n << std::endl;
}

void f(push_type &yield) {
    log(1);
    yield(3);
    log(5);
}

int main() {
    log(0);
    pull_type source(f);
    log(2);
    log(source.get());
    log(4);
    source();
}