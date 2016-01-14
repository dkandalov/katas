#![allow(dead_code,unused_variables)]
fn main() {
    // 8
    // (http://rustbyexample.com/fn.html)
    fn is_divisable_by(lhs: u32, rhs: u32) -> bool {
        if rhs == 0 {
            return false;
        }
        lhs % rhs == 0
    }
    fn void_return(n: u32) -> () {}
    fn void_return2(n: u32) {}


    // 8.1
    // (http://rustbyexample.com/fn/methods.html)
    struct Point {
        x: f64,
        y: f64,
    }
    impl Point {
        fn origin() -> Point { // static method
            Point{ x: 0.0, y: 0.0 }
        }
        fn new(x: f64, y: f64) -> Point { // static method
            Point{ x: x, y: y }
        }
    }
    struct Rectangle {
        p1: Point,
        p2: Point,
    }
    impl Rectangle {
        // `&self` is sugar for `self: &Self`, where `Self` is the type of the
        // caller object. In this case `Self` = `Rectangle`
        fn area(&self) -> f64 {
            let Point{ x: x1, y: y1 } = self.p1;
            let Point{ x: x2, y: y2 } = self.p2;
            2.0 * ((x1- x2).abs() + (y1 - y2).abs())
        }
        fn translate(&mut self, x: f64, y: f64) {
            self.p1.x += x;
            self.p2.x += x;
            self.p1.y += y;
            self.p2.y += y;
        }
    }


    // 8.2
    // (http://rustbyexample.com/fn/closures.html)
    fn function(i: i32) -> i32 { i + 1 }
    let closure1 = |i: i32| -> i32 { i + 1 };
    let closure2 = |i| i + 1;
    let i = 1;
    println!("function: {}", function(i));
    println!("closure1: {}", closure1(i));
    println!("closure2: {}", closure2(i));
    let one = || 1;
    println!("closure returning one: {}", one());


    // 8.2.1
    // (http://rustbyexample.com/fn/closures/capture.html)
    let s = "string"; // capture by reference: &T
    let print_s = || println!("s: {}", s);
    print_s();

    let mut count = 0; // capture by mutable reference: &mut T
    let mut inc = || { // 'mut' is required
        count += 1;
        println!("count: {}", count);
    };
    inc();
    inc();
    // error: cannot borrow `count` as mutable more than once at a time [E0499]
    // let reborrow = &mut count;

    let movable = Box::new(3); // capture by value
    let consume = || {
        println!("movable: {}", movable);
        std::mem::drop(movable);
    };
    consume();
    // error: use of moved value: `consume` [E0382]
    // consume();


    // 8.2.2
    // (http://rustbyexample.com/fn/closures/input_parameters.html)
    // FnOnce: takes captures by value (T)
    fn apply<F>(f: F) where F: FnOnce() {
        f()
    }
    // Fn: takes captures by reference (&T)
    fn apply_to_3<F>(f: F) -> i32 where F: Fn(i32) -> i32 {
        f(3)
    }
    let greeting = "hello";
    let mut farewell = "goodbye".to_owned();
    // TODO
}
