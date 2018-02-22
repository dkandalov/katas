#include <iostream>
#include <future>
#include <experimental/coroutine>

std::future<int> async_add(int a, int b) {
    auto fut = std::async([=]() {
        int c = a + b;
        return c;
    });
    return fut;
}

std::future<int> async_fib(int n) {
    if (n <= 2) co_return 1;

    int a = 1;
    int b = 1;

    // iterate computing fib(n)
    for (int i = 0; i < n - 2; ++i) {
        int c = co_await async_add(a, b);
        a = b;
        b = c;
    }

    co_return b;
}

int main() {
    auto f = async_add(1, 2);
    std::cout << f.get() << std::endl;
}