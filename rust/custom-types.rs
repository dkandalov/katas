
#[allow(dead_code,unused_variables,non_upper_case_globals)]
fn main() {
    // 3.1
    struct Nil; // a unit struct
    struct Pair(i32, f64);
    struct Point {
      x: f64,
      y: f64,
    }
    struct Rectangle {
        p1: Point,
        p2: Point,
    }
    let point = Point{x: 0.3, y: 0.4};
    println!("point coordinates: ({}, {})", point.x, point.y);
    let Point { x: my_x, y: my_y } = point; // destructure point
    let rectangle = Rectangle {
        p1: Point { x: my_x, y: my_y },
        p2: point,
    };
    let nil = Nil;
    let pair = Pair(1, 0.1);
    let Pair(integer, decimal) = pair;
    println!("pair contains {:?} and {:?}", integer, decimal);

    // 3.2
    fn inspect(p: Person) {
        match p {
            Person::Skinny  => println!("Is skinny!"),
            Person::Fat     => println!("Is fat!"),
            Person::Height(i) => println!("Has height of {}.", i),
            Person::Weight(i) => println!("Has weight of {}.", i),
            Person::Info{ name, height } => {
                println!("{} is {} tall!", name, height);
            }
        }
    }
    let person = Person::Height(18);
    inspect(person);

    // 3.2.1
    {
        // "use" has to be first statement, Person must be defined outside this function
        use Person::*;
        inspect(Info{ name: "Dave".to_owned(), height: 72 });
    }

    // 3.2.2
    enum Number {
        Zero, One, Two
    }
    enum Color {
        Red = 0xff0000,
        Green = 0x00ff00,
        Blue = 0x0000ff,
    }
    println!("roses are #{:06x}", Color::Red as i32);

    // 3.2.3
    {
        enum List {
            Cons(u32, Box<List>),
            Nil
        }
        impl List {
            fn new() -> List {
                List::Nil
            }
            fn prepend(self, elem: u32) -> List {
                List::Cons(elem, Box::new(self))
            }
            fn len(&self) -> u32 {
                match *self {
                    List::Cons(_, ref tail) => 1 + tail.len(),
                    List::Nil => 0
                }
            }
            fn stringify(&self) -> String {
                match *self {
                    List::Cons(head, ref tail) => {
                        format!("{}, {}", head, tail.stringify())
                    },
                    List::Nil => {
                        format!("Nil")
                    }
                }
            }
        }
        let mut list = List::new();
        list = list.prepend(1);
        list = list.prepend(2);
        list = list.prepend(3);
        println!("linked list length: {}", list.len());
        println!("{}", list.stringify());
    }

    // 3.3
    const threshold: i32 = 10;
    // threshold = 11; // error: invalid left-hand side expression [E0070]
    static language: &'static str = "Rust";
}

#[allow(dead_code,unused_variables)]
enum Person {
    Skinny,
    Fat,
    Height(i32),
    Weight(i32),
    Info { name: String, height: i32 }
}
