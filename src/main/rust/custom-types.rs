#[allow(dead_code)]
fn main() {
    struct Nil;

    struct Pair(i32, f64);

    struct Point {
      x: f64;
      y: f64;
    }

    struct Rectangle {
        p1: Point;
        p2: Point;
    }

    let point = Point{x: 0.3, y: 0.4};
    println!("point coordinates: ({}, {})", point.x, point.y);
}
