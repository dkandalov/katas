#include <iostream>
#include <future>
#include <experimental/coroutine>
//#include <experimental/future>

template<typename R, typename... Args>
struct std::experimental::coroutine_traits<std::future<R>, Args...> {
    struct promise_type {
        std::promise <R> p;
        auto get_return_object() { return p.get_future(); }
        bool await_ready() { return false; }
        std::experimental::suspend_never initial_suspend() { return {}; }
        std::experimental::suspend_never final_suspend() { return {}; }
        void set_exception(std::exception_ptr e) { p.set_exception(std::move(e)); }
        template<typename U> void return_value(U &&u) { p.set_value(std::forward<U>(u)); }
        void unhandled_exception() { std::terminate(); }
    };
};

template <typename R>
auto operator co_await(std::future<R> &&f) {
    struct Awaiter {
        std::future<R> &&input;
        std::future<R> output;
        bool await_ready() { return false; }
        auto await_resume() { return output.get(); }
        void await_suspend(std::experimental::coroutine_handle<> coro) {
//            input.then([this, coro](auto result_future) {
//                this->output = std::move(result_future);
//                coro.resume();
//            });
            this->output = std::move(input);
            coro.resume();
        }
    };
    return Awaiter{static_cast<std::future<R>&&>(f)};
}

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
    for (int i = 0; i < n - 2; ++i) {
        int c = co_await async_add(a, b);
        a = b;
        b = c;
    }
    co_return b;
}

/*
generator<int> generatorForNumbers(int begin, int inc = 1) {
    for (int i = begin;; i += inc) {
        co_yield i;
    }
}
*/

int main() {
    auto fib = async_fib(5);
    std::cout << fib.get();

//    auto f = async_add(1, 2);
//    std::cout << f.get() << std::endl;
}