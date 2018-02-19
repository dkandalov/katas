#include <iostream>
#include <boost/coroutine/all.hpp>

namespace two {
    typedef boost::coroutines::asymmetric_coroutine<int>::pull_type pull_type;
    typedef boost::coroutines::asymmetric_coroutine<int>::push_type push_type;

    void log(int n) {
        std::cout << "n = " << n << std::endl;
    }

    void f(push_type &yield) {
        log(1);
        yield(2);
        log(3);
    }

    void main() {
        log(0);
        pull_type source(f);
        log(source.get());
    }
}
