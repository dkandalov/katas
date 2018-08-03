#![allow(unused_variables, overflowing_literals)]

fn main() {
    // 5
    let decimal = 65.4321_f32;
    // error: mismatched types (because there is no implicit conversion)
    //let integer: u8 = decimal;

    let integer = decimal as u8;
    let character = integer as char;
    println!("Casting: {} -> {} -> {}", decimal, integer, character);

    println!("1000 as u16: {}", 1000 as u16); // 1000
    // 1000 - 256 - 256 - 256 = 232
    println!("1000 as u8: {}", 1000 as u8); // 232
    println!("-1 as u8: {}", (-1i8) as u8); // 255

    // 5.1
    let i = 1; // i32 unless there are constraints in how varible is used
    let f = 1.0; // f64 unless there are constraints
    println!("size of i: {}, f: {}", std::mem::size_of_val(&i), std::mem::size_of_val(&f));

    // 5.2
    let elem = 5u8;
    let mut vec = Vec::new();
    vec.push(elem); // inferred type of Vec<u8>
    println!("{:?}", vec);

    // 5.3
    type NanoSecond = u64;
    type Inch = u64;
    let nanoseconds: NanoSecond = 5;
    let inches: Inch = 2;
    let sum = nanoseconds + inches; // type aliases are NOT new types
}
