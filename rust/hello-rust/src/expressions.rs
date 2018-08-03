fn main() {
    let x = 5u32;
    let y = {
        x * x // return value of block
    };
    let z = {
        2 * x; // ";" makes the block return "()" void
    };
    println!("x: {:?}, y: {:?}, z: {:?}", x, y, z);
}
