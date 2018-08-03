use std::fmt::{Display, Formatter, Result};
use std::mem;

#[allow(dead_code, unused_variables, unused_mut)]
fn main() {
    let an_integer0 = 5i8;
    let an_integer1 = 5i16;
    let an_integer2 = 5i32;
    let an_integer3 = 5i64;
    let unsigned_integer0 = 5u8;
    let unsigned_integer1 = 5u16;
    let unsigned_integer2 = 5u32;
    let unsigned_integer3 = 5u64;
    let a_float0: f32 = 1.0;
    let a_float1: f64 = 1.0;
    let char0: char = 'a';
    let char1 = 'α';
    let char2 = '∞'; // 4 bytes
    let default_float = 3.0; // f64
    let default_integer = 7; // i32
    let mut mutable = 12; // mutable i32
    // mutable = true; // type of variable can't be changed
    let logical: bool = true;
    let unit: () = ();
    let array: [i32; 3] = [1, 2, 3];
    let tuple: (i32, bool) = (1, true);

    // 2.1
    println!("0011 AND 0101 is {:04b}", 0b0011u32 & 0b0101);
    println!("0011 OR 0101 is {:04b}", 0b0011u32 | 0b0101);
    println!("0011 XOR 0101 is {:04b}", 0b0011u32 ^ 0b0101);
    println!("1 << 5 is {}", 1u32 << 5);
    println!("0x80 >> 2 is 0x{:x}", 0x82u32 >> 2);
    println!("One million: {}", 1_000_000u32);

    // 2.2
    fn reverse(pair: (i32, bool)) -> (bool, i32) {
        let (i, b) = pair;
        (b, i)
    }
    #[derive(Debug)]
    struct Matrix(f32, f32, f32, f32);
    let tuple_of_tuples = ((1u8, 2u16, 3u32), (4u64, -1i8), -2i16);
    println!("tuple of tuples {:?}", tuple_of_tuples);
    println!("tuple 1: {:?}", tuple_of_tuples.0);

    let tuple: (i32, &str, f64, bool) = (1, "hello", 4.5, true);
    let (a, b, c, d) = tuple;
    println!("{:?} {:?} {:?} {:?}", a, b, c, d);

    let matrix = Matrix(1.1, 1.2, 2.1, 2.2);
    println!("{:?}", matrix);
    impl Display for Matrix {
        fn fmt(&self, f: &mut Formatter) -> Result {
            write!(f, "( {} {} )\n( {} {} )", self.0, self.1, self.2, self.3)
        }
    }
    println!("{}", matrix);

    fn transpose(m: Matrix) -> Matrix {
        return Matrix(m.0, m.2, m.1, m.3);
    }
    println!("Transpose:\n{}", transpose(matrix));

    // 2.3
    let xs: [i32; 5] = [1, 2, 3, 4, 5];
    let ys: [i32; 500] = [0; 500]; // init all elements to 0
    println!("first element of the array: {}", xs[1]);
    println!("second element of the array: {}", xs[2]);
    //println!("array out of bound: {}", xs[5]);
    println!("array size: {}", xs.len());
    println!("array occupies {} bytes", mem::size_of_val(&xs));
    fn analyze_slice(slice: &[i32]) {
        println!("slice[0]: {}, size: {}", slice[0], slice.len());
    }
    analyze_slice(&xs);
    analyze_slice(&ys[1..4]);
}
