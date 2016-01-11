#![allow(dead_code,unused_variables)]
fn main() {
    // 7.1
    let n = 5;
    let big_n =
        if n < 10 && n > -10 {
            10 * n
        } else {
            n / 2
        };
    println!("{} -> {}", n, big_n);

    // 7.2
    println!("loop{{}}");
    let mut count = 0u32;
    loop {
        count += 1;
        println!("{:?}", count);
        if count == 5 {
            println!("break");
            break;
        }
    }

    // 7.2.1
    'outer: loop  {
        println!("outer loop");
        'inner: loop {
            println!("inner loop");
            break 'outer;
        }
    }
    println!("exit outer loop");

    // 7.3
    let mut n = 1;
    while n <= 3 {
        n += 1;
    }
    println!("while: {:?}", n);

    // 7.4
    for n in 1..3 {
        println!("for: {:?}", n);
    }

    // 7.5
    let number = 13;
    match number {
        1 => println!("One!"),
        2 | 3 | 5 | 7 | 11 => println!("This is prime"),
        13...19 => println!("13...19 range"),
        _ => println!("default"),
    }

    // 7.5.1.1
    let pair = (0, -2);
    match pair {
        (0, y) => println!("(0, {})", y),
        (x, 0) => println!("({}, 0)", x),
        _ => println!("default"),
    }

    // 7.5.1.2
    enum Color {
        Red, Blue, Green, RGB(u32, u32, u32)
    }
    let color = Color::Blue;
    match color {
        Color::Red => println!("Red"),
        Color::Green => println!("Green"),
        Color::Blue => println!("Blue"),
        Color::RGB(r,g,b) => println!("RGB"),
    }

    // 7.5.1.3
    let reference = &4;
    match reference {
        &val => println!("Value via destructing: {:?}", val),
    }
    match *reference { // deference before matching
        val => println!("Value via dereferencing: {:?}", val),
    }
    let ref a_reference = 3;
    let value = 5;
    match value {
        ref r => println!("Got reference value: {}", r),
    }
    let mut mut_value = 6;
    match mut_value {
        ref mut m => {
            *m += 10;
            println!("mut_value: {}", m);
        },
    }
    struct Foo { x: (u32, u32), y: u32 }
    let foo = Foo { x: (1,2), y: 3 };
    let Foo { y, .. } = foo; // some destructed values can be ignored
    println!("foo y: {:?}", y);

    // 7.5.2
    let pair = (2, -2);
    match pair {
        (x, y) if x == y => println!("x == y"),
        (x, _) => println!("pair x: {:?}", x),
    }

    // 7.5.3
    fn age() -> u32 {
        15
    }
    match age() {
        0 => println!("age zero"),
        n @ 1...22 => println!("age 1...22: {}", n),
        _ => println!("default"),
    }

    // 7.6
    let mut optional: Option<i32> = Some(7);
    match optional {
        Some(i) => println!("i: {:?}", i),
        _ => {},
    }
    if let Some(i) = optional {
        println!("Matched number: {}", i);
    } else {
        println!("No match");
    }
    while let Some(i) = optional {
        println!("while let {{ {} }}", i);
        if i > 7 {
            break;
        }
        optional = Some(i + 1);
    }
}
