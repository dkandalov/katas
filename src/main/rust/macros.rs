fn main() {
    // 9
    macro_rules! say_hello {
        () => (
            // macro will expand into the content of this block
            println!("Hello!");
        )
    }
    say_hello!();


    // 9.1
    macro_rules! create_function {
        ($func_name:ident) => (
            fn $func_name() {
                println!("Called function: {}()", stringify!($func_name))
            }
        )
    }
    create_function!(foo);
    create_function!(bar);
    foo();
    bar();

    macro_rules! print_result {
        ($expression:expr) => (
            println!("{:?} = {:?}", stringify!($expression), $expression)
        );
    }
    print_result!({1u32 + 1});
    print_result!({
        let x = 1u32;
        x * x + 2 * x - 1
    });
}
