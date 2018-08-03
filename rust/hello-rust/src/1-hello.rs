use std::fmt::{self, Formatter, Display};

fn main() {
    println!("Hello World!");

    let x = 5 + /* 90 + */ 5;
    println!("Is `x` 10 or 100? x = {}", x);

    // 1.2
    println!("{} days", 31);

    println!("{0}, this is {1}. {1}, this is {0}", "Alice", "Bob");
    println!("{subject} {verb} {predicate}",
             predicate = "over the lazy dog",
             subject = "the quick brown fox",
             verb = "jumps");
    println!("{} of {:b} people know binary", 1, 2);
    println!("{number:>width$}", number = 1, width = 6);
    println!("{number:>0width$}", number = 1, width = 6);

    // println!("My name is {0}, {1} {0}", "Bond");
    // <std macros>:3:11: 3:36 error: invalid reference to argument `1` (there is 1 argument)

    //struct Structure(i32);
    // println!("This struct `{}` won't print...", Structure(3));
    // hello.rs:22:49: 22:61 error: the trait `core::fmt::Display` is not implemented for the type `main::Structure` [E0277]

    #[allow(dead_code)]
    struct UnPrintable(i32);
    #[derive(Debug)]
    struct Structure(i32);
    #[derive(Debug)]
    struct Deep(Structure);

    // 1.2.1
    println!("{:?} months in a year.", 12);
    println!("{1:?} {0:?} is the {actor:?} name",
             "Slater", "Christian", actor = "actor's");
    println!("Now {:?} will print!", Structure(3));
    println!("Now {:?} will print!", Deep(Structure(7)));

    // 1.2.2
    // need to implement Display to use '{}' marker.
    impl fmt::Display for Structure {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }
    println!("Now {} will print!", Structure(12));

    #[derive(Debug)]
    struct MinMax(i64, i64);
    impl fmt::Display for MinMax {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "({}, {})", self.0, self.1)
        }
    }
    #[derive(Debug)]
    struct Point2 {
        x: f64,
        y: f64,
    }
    impl fmt::Display for Point2 {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "x: {}, y: {}", self.x, self.y)
        }
    }

    let minmax = MinMax(0, 14);
    println!("Compare structures:");
    println!("Display: {}", minmax);
    println!("Debug: {:?}", minmax);

    let big_range = MinMax(-300, 300);
    let small_range = MinMax(-3, 3);
    println!("The big range is {big} and the small is {small}",
             small = small_range, big = big_range);

    let point = Point2 { x: 3.3, y: 7.2 };
    println!("Compare points:");
    println!("Display: {}", point);
    println!("Debug: {:?}", point);
    // hello.rs:79:30: 79:35 error: the trait `core::fmt::Binary` is not implemented for the type `main::Point2` [E0277]
    // println!("Binary: {:b}", point);

    // 1.2.2.1
    struct List(Vec<i32>);
    impl fmt::Display for List {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let List(ref vec) = *self;
            try!(write!(f, "["));
            for (count, v) in vec.iter().enumerate() {
                if count != 0 { try!(write!(f, ", ")); }
                try!(write!(f, "{}", v));
            }
            write!(f, "]")
        }
    }
    let v = List(vec![1, 2, 3]);
    println!("{}", v);

    // 1.2.3
    struct City {
        name: &'static str,
        lat: f32,
        lon: f32,
    }
    impl fmt::Display for City {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let lat_c = if self.lat >= 0.0 { 'N' } else { 'S' };
            let lon_c = if self.lon >= 0.0 { 'E' } else { 'W' };
            write!(f, "{}: {:.3}°{} {:.3}°{}", self.name, self.lat.abs(), lat_c, self.lon.abs(), lon_c)
        }
    }
    #[derive(Debug)]
    struct Color {
        red: u8,
        green: u8,
        blue: u8,
    }
    for city in [
        City { name: "Dublin", lat: 53.347778, lon: -6.259722 },
        City { name: "Oslo", lat: 59.95, lon: 10.75 },
        City { name: "Vancouver", lat: 49.25, lon: -123.1 },
    ].iter() {
        println!("{}", *city);
    }
    for color in [
        Color { red: 128, green: 255, blue: 90 },
        Color { red: 0, green: 3, blue: 254 },
        Color { red: 0, green: 0, blue: 0 },
    ].iter() {
        println!("{:?}", *color);
    }
}
