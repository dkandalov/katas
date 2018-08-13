extern crate libc;

use std::ffi::{CString, CStr};

#[link(name = "ncurses")]
extern "C" {
    fn getpid() -> i32;
    fn initscr() -> *mut i8;
    fn noecho() -> i32;
    fn curs_set(c: i8) -> i32;
    fn halfdelay(tenths: i8) -> i32;
    #[link_name = "box"]
    fn box_(window: *mut i8, verch: i8, horch: i8);
    fn mvprintw(y: i8, x: i8, s: *const libc::c_char);
    fn getch() -> i8;
    fn endwin() -> libc::c_int;

    fn newwin(nlines: i8, ncols: i8, begin_y: i8, begin_x: i8) -> *mut i8;
    fn wclear(window: *mut i8) -> i8;
    fn wrefresh(window: *mut i8) -> i8;
    fn mvwprintw(window: *mut i8, y: i8, x: i8, s: *const libc::c_char);
    fn wgetch(window: *mut i8) -> i8;
    fn delwin(window: *mut i8) -> i8;
}

trait ToCStr {
    fn to_c_str(&self) -> CString;
}

fn main() {
    unsafe {
        initscr();
        noecho();
        curs_set(0);
        halfdelay(3);

        let width = 20;
        let height = 10;
        let window = newwin(height + 2, width + 2, 0, 0);

        let mut snake = Snake {
            cells: vec![Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }, Cell { x: 0, y: 0 }],
            direction: Direction::Right,
        };

        let mut c = 0;
        while c != 'q' as i8 {
            wclear(window);
            box_(window, 0, 0);
            mvwprintw(window, snake.head().y + 1, snake.head().x + 1, "Q".to_c_str().as_ptr());
            snake.tail().iter().for_each(|cell|
                mvwprintw(window, cell.y + 1, cell.x + 1, "o".to_c_str().as_ptr())
            );

            wrefresh(window);

            c = wgetch(window);

            snake = snake.slide();
        }

        delwin(window);
        endwin();
    }
}

impl<'a> ToCStr for &'a str {
    fn to_c_str(&self) -> CString {
        CString::new(*self).unwrap()
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Snake {
    cells: Vec<Cell>,
    direction: Direction,
}

impl Snake {
    fn head(&self) -> &Cell {
        &self.cells[0]
    }

    fn tail(&self) -> Vec<Cell> {
        self.cells[1..self.cells.len()].to_owned()
    }

    fn slide(&self) -> Snake {
        let mut vec: Vec<Cell> = vec![self.cells.first().unwrap().move_in(&self.direction)];

        let mut new_cells = self.cells.clone();
        new_cells.pop();
        vec.append(&mut new_cells);

        Snake { cells: vec, direction: self.direction }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct Cell {
    x: i8,
    y: i8,
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
    fn snake_moves_right() {
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