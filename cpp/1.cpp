#include <iostream>
#include <boost/coroutine2/all.hpp>

typedef boost::coroutines2::coroutine<void>::pull_type pull_type;
typedef boost::coroutines2::coroutine<void>::push_type push_type;

void log(int n) {
    std::cout << "n = " << n << std::endl;
}

void f(push_type &yield) {
    log(1);
    yield();
    log(3);
}

int main() {
    log(0);
    pull_type source(f);
    log(2);
    source();
    log(4);
}