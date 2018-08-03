fn main() {
    let an_integer = 1u32;
    // because name starts with "_", no warning that variable is unused
    let _unused_variable = an_integer;

    let mut mutable = 1;
    mutable += 1;
    println!("{}", mutable);

    let a_binding;
    {
        let x = 2;
        a_binding = x * x;
    }
    println!("a binding: {}", a_binding);

    let another_binding: i32;
    // error: use of possibly uninitialized variable: `another_binding` [E0381]
    // println!("another binding: {}", another_binding);
}
