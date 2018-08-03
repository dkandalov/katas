extern crate libc;

use std::ffi::{CString, CStr};

#[link(name = "ncurses")]
extern "C" {
    fn getpid() -> i32;
    fn initscr() -> *mut i8;
    fn noecho() -> i32;
    fn curs_set(c: i8) -> i32;
    #[link_name = "box"]
    fn box_(window: *mut i8, verch: i8, horch: i8);
    fn mvprintw(y: i8, x: i8, s: *const libc::c_char);
    fn getch() -> *mut i8;
    fn endwin() -> libc::c_int;
}

trait ToCStr {
    fn to_c_str(&self) -> CString;
}

impl<'a> ToCStr for &'a str {
    fn to_c_str(&self) -> CString {
        CString::new(*self).unwrap()
    }
}

fn main() {
    unsafe {
        let window = initscr();
        noecho();
        curs_set(0);

        let mut c = 0;
        mvprintw(3, 2, "hello 22".to_c_str().as_ptr());
        box_(window, 0, 0);
        getch();

        endwin();
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Snake {
    cells: Vec<Cell>,
    direction: Direction,
}

impl Snake {
    fn slide(&self) -> Snake {
        let mut vec: Vec<Cell> = vec![self.cells.first().unwrap().move_in(&self.direction)];

        let mut new_cells = self.cells.clone();
        new_cells.pop();
        vec.append(&mut new_cells);

        Snake { cells: vec, direction: self.direction }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Cell {
    x: i32,
    y: i32,
}

impl Cell {
    fn move_in(&self, direction: &Direction) -> Cell {
        let dx = match direction {
            Direction::Up => 0,
            Direction::Down => 0,
            Direction::Left => -1,
            Direction::Right => 1,
        };
        let dy = match direction {
            Direction::Up => -1,
            Direction::Down => 1,
            Direction::Left => 0,
            Direction::Right => 0,
        };
        return Cell { x: self.x + dx, y: self.y + dy };
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let snake = Snake {
            cells: vec![Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }, Cell { x: 0, y: 0 }],
            direction: Direction::Right,
        };

        assert_eq!(
            snake.slide(),
            Snake {
                cells: vec![Cell { x: 3, y: 0 }, Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }],
                direction: Direction::Right,
            }
        )
    }
}