#include <iostream>
#include <boost/context/all.hpp>

using namespace boost::context;

void log(int n) {
    std::cout << "n = " << n << std::endl;
}

int main() {
    log(0);
    continuation f = callcc([](continuation &&c) {
        log(1);
        c = c.resume();
        log(3);
        return std::move(c);
    });
    log(2);
    f.resume();
    log(4);
}