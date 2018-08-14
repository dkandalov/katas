extern crate libc;

use std::ffi::{CString};

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
        while char::from(c as u8) != 'q' {
            wclear(window);
            box_(window, 0, 0);
            mvwprintw(window, snake.head().y + 1, snake.head().x + 1, "Q".to_c_str().as_ptr());
            snake.tail().iter().for_each(|cell|
                mvwprintw(window, cell.y + 1, cell.x + 1, "o".to_c_str().as_ptr())
            );

            wrefresh(window);

            c = wgetch(window);
            let direction = match char::from(c as u8) {
                'i' => Some(Direction::Up),
                'j' => Some(Direction::Left),
                'k' => Some(Direction::Down),
                'l' => Some(Direction::Right),
                _ => Option::None
            };

            if direction.is_some() {
                snake = snake.turn_in(direction.unwrap())
            }
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

    fn turn_in(&self, new_direction: Direction) -> Snake {
        if are_opposite(new_direction, self.direction) {
            self.clone()
        } else {
            Snake { cells: self.cells.clone(), direction: new_direction }
        }
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

fn are_opposite(d1: Direction, d2: Direction) -> bool {
    if d1 == Direction::Up && d2 == Direction::Down { return true }
    if d2 == Direction::Up && d1 == Direction::Down { return true }
    if d1 == Direction::Left && d2 == Direction::Right { return true }
    if d2 == Direction::Left && d1 == Direction::Right { return true }
    false
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

    #[test]
    fn snake_can_change_direction() {
        let snake = Snake {
            cells: vec![Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }, Cell { x: 0, y: 0 }],
            direction: Direction::Right,
        };

        assert_eq!(
            snake.turn_in(Direction::Down).slide(),
            Snake {
                cells: vec![Cell { x: 2, y: 1 }, Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }],
                direction: Direction::Down,
            }
        );
        assert_eq!(
            snake.turn_in(Direction::Left).slide(),
            Snake {
                cells: vec![Cell { x: 3, y: 0 }, Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }],
                direction: Direction::Right,
            }
        );
    }
}