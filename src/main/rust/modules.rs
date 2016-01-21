mod module {
    #[allow(dead_code)]
    fn private_function() {
        println!("Called private_function");
    }
    pub fn public_function() {
        println!("Called public_function");
    }
    pub mod nested {
        pub fn function(postfix: &str) {
            println!("called nested module function ({})", postfix);
        }
    }

    pub struct WhiteBox<T> {
        pub contents: T
    }
    pub struct BlackBox<T> {
        #[allow(dead_code)]
        contents: T
    }
    impl<T> BlackBox<T> {
        pub fn new(contents: T) -> BlackBox<T> {
            BlackBox{ contents: contents }
        }
    }
}

#[allow(unused_variables)]
fn main() {
    // 10.1
    module::public_function();
    module::nested::function("");

    // 10.2
    let white_box = module::WhiteBox{ contents: "public information" };
    println!("white_box contents: {}", white_box.contents);
    let black_box = module::BlackBox::new("classified information");

    // 10.3
    {
        use module::nested::function as f;
        f("use")
    }
}
