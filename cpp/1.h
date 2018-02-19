#include <iostream>
#include <boost/context/all.hpp>
#include <boost/coroutine/all.hpp>

namespace one {
    typedef boost::coroutines::asymmetric_coroutine<void>::pull_type pull_type;
    typedef boost::coroutines::asymmetric_coroutine<void>::push_type push_type;

    void log(int n) {
        std::cout << "n = " << n << std::endl;
    }

    void f(push_type &yield) {
        log(1);
        yield();
        log(3);
    }

    void main() {
        log(0);
        pull_type source(f);
        log(2);
        source();
        log(4);
    }
}
