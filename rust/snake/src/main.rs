extern crate libc;
extern crate rand;
use rand::Rng;
use rand::ChaChaRng;
use rand::SeedableRng;
use std::ffi::{CString};
use rand::rngs::EntropyRng;

#[link(name = "ncurses")]
#[allow(dead_code)]
extern "C" {
    fn getpid() -> i32;
    fn initscr() -> *mut i8;
    fn noecho() -> i32;
    fn curs_set(c: i8) -> i32;
    fn halfdelay(tenths: i8) -> i32;
    #[link_name = "box"]
    fn box_(window: *mut i8, verch: i8, horch: i8);
    fn mvprintw(y: i16, x: i16, s: *const libc::c_char);
    fn getch() -> i8;
    fn endwin() -> libc::c_int;

    fn newwin(nlines: i16, ncols: i16, begin_y: i8, begin_x: i8) -> *mut i8;
    fn wclear(window: *mut i8) -> i8;
    fn wrefresh(window: *mut i8) -> i8;
    fn mvwprintw(window: *mut i8, y: i16, x: i16, s: *const libc::c_char);
    fn wgetch(window: *mut i8) -> i8;
    fn delwin(window: *mut i8) -> i8;
}

fn main() {
    unsafe {
        initscr();
        noecho();
        curs_set(0);
        halfdelay(3);

        let width = 20;
        let height = 10;
        let mut game = Game {
            width,
            height,
            snake: Snake::new(
                vec![Cell { x: 4, y: 0 }, Cell { x: 3, y: 0 }, Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }, Cell { x: 0, y: 0 }],
                Direction::Right
            ),
            apples: Apples::create(width, height)
        };
        let window = newwin(game.height + 2, game.width + 2, 0, 0);

        let mut c = 0;
        while char::from(c as u8) != 'q' {

            game.draw(window);

            c = wgetch(window);
            let direction = match char::from(c as u8) {
                'i' => Some(Direction::Up),
                'j' => Some(Direction::Left),
                'k' => Some(Direction::Down),
                'l' => Some(Direction::Right),
                _ => Option::None
            };

            game = game.update(direction);
        }

        delwin(window);
        endwin();
    }
}

impl Game {
    fn draw(&self, window: *mut i8) {
        unsafe {
            wclear(window);
            box_(window, 0, 0);

            self.apples.cells.iter().for_each(|cell|
                mvwprintw(window, cell.y + 1, cell.x + 1, ".".to_c_str().as_ptr())
            );
            self.snake.tail().iter().for_each(|cell|
                mvwprintw(window, cell.y + 1, cell.x + 1, "o".to_c_str().as_ptr())
            );
            mvwprintw(window, self.snake.head().y + 1, self.snake.head().x + 1, "Q".to_c_str().as_ptr());

            if self.is_over() {
                mvwprintw(window, 0, 4, "Game is Over".to_c_str().as_ptr());
                mvwprintw(window, 1, 3, format!("Your score is {}", self.score()).as_str().to_c_str().as_ptr());
            }

            wrefresh(window);
        }
    }
}

#[derive(Debug, Clone)]
struct Game {
    width: i16,
    height: i16,
    snake: Snake,
    apples: Apples
}

impl Game {
    fn is_over(&self) -> bool {
        self.snake.cells.iter().any(|&it| it.x < 0 || it.x >= self.width || it.y < 0 || it.y >= self.height) ||
        self.snake.tail().contains(self.snake.head())
    }

    fn score(&self) -> u8 {
        self.snake.cells.len() as u8
    }

    fn update(&self, direction: Option<Direction>) -> Game {
        if self.is_over() {
            return self.clone();
        }

        let new_snake;
        if direction.is_some() {
            new_snake = self.snake.turn_in(direction.unwrap()).slide();
        } else {
            new_snake = self.snake.slide();
        }
        let new_apples = self.apples.clone().grow().clone(); // TODO

        let (new_snake2, new_apples2) = new_snake.eat(new_apples);

        return Game {
            width: self.width,
            height: self.height,
            snake: new_snake2,
            apples: new_apples2
        };
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Snake {
    cells: Vec<Cell>,
    direction: Direction,
    eaten_apples: i16
}

impl Snake {
    fn new(cells: Vec<Cell>, direction: Direction) -> Snake {
        Snake { cells, direction, eaten_apples: 0 }
    }

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
            Snake { cells: self.cells.clone(), direction: new_direction, eaten_apples: self.eaten_apples }
        }
    }

    fn slide(&self) -> Snake {
        // TODO cleanup
        let mut vec: Vec<Cell> = vec![self.cells.first().unwrap().move_in(&self.direction)];

        let mut new_cells = self.cells.clone();
        if self.eaten_apples == 0 {
            new_cells.pop();
        }
        vec.append(&mut new_cells);

        Snake { cells: vec, direction: self.direction, eaten_apples: std::cmp::max(0, self.eaten_apples - 1) }
    }

    fn eat(&self, apples: Apples) -> (Snake, Apples) {
        if apples.cells.contains(self.head()) {
            let mut new_apple_cells = apples.cells.clone();
            new_apple_cells.retain(|it| it != self.head());
            let new_apples = apples.with_cells(new_apple_cells);

            let new_snake = Snake {
                cells: self.cells.clone(),
                direction: self.direction,
                eaten_apples: self.eaten_apples + 1
            };

            (new_snake, new_apples)
        } else {
            (self.clone(), apples.clone())
        }
    }
}

#[derive(Debug, Clone)]
struct Apples {
    field_width: i16,
    field_height: i16,
    cells: Vec<Cell>,
    growth_speed: i16,
    rng: ChaChaRng
}

impl Apples {
    fn create(field_width: i16, field_height: i16) -> Apples {
        let apples = Apples {
            field_width,
            field_height,
            cells: vec![],
            growth_speed: 3,
            rng: ChaChaRng::from_rng(EntropyRng::new()).unwrap()
        };
        return apples;
    }

    fn with_cells(self, cells: Vec<Cell>) -> Apples {
        Apples { cells, ..self }
    }

    fn grow(&mut self) -> &mut Apples {
        let n = self.rng.gen_range(0, self.growth_speed);
        if n != 0 {
            return self
        }
        let cell = Cell {
            x: self.rng.gen_range(0, self.field_width),
            y: self.rng.gen_range(0, self.field_height)
        };
        self.cells.push(cell);

        return self;
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct Cell {
    x: i16,
    y: i16,
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
    if d1 == Direction::Up && d2 == Direction::Down { return true; }
    if d2 == Direction::Up && d1 == Direction::Down { return true; }
    if d1 == Direction::Left && d2 == Direction::Right { return true; }
    if d2 == Direction::Left && d1 == Direction::Right { return true; }
    false
}

trait ToCStr {
    fn to_c_str(&self) -> CString;
}

impl<'a> ToCStr for &'a str {
    fn to_c_str(&self) -> CString {
        CString::new(*self).unwrap()
    }
}


#[cfg(test)]
mod snake_tests {
    use super::*;

    #[test]
    fn snake_moves_right() {
        let snake = Snake::new(
            vec![Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }, Cell { x: 0, y: 0 }],
            Direction::Right,
        );

        assert_eq!(
            snake.slide(),
            Snake::new(
                vec![Cell { x: 3, y: 0 }, Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }],
                Direction::Right,
            )
        )
    }

    #[test]
    fn snake_can_change_direction() {
        let snake = Snake::new(
            vec![Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }, Cell { x: 0, y: 0 }],
            Direction::Right,
        );

        assert_eq!(
            snake.turn_in(Direction::Down).slide(),
            Snake::new(
                vec![Cell { x: 2, y: 1 }, Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }],
                Direction::Down
            )
        );
        assert_eq!(
            snake.turn_in(Direction::Left).slide(),
            Snake::new(
                vec![Cell { x: 3, y: 0 }, Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }],
                Direction::Right
            )
        );
    }

    #[test]
    fn snake_eats_an_apple() {
        let snake = Snake::new(
            vec![Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }, Cell { x: 0, y: 0 }],
            Direction::Right
        );
        let apples = Apples::create(10, 10).with_cells(vec![Cell { x: 2, y: 0 }]);

        let (new_snake, new_apples) = snake.eat(apples);
        assert_eq!(new_apples.cells, vec![]);
        assert_eq!(new_snake.eaten_apples, 1);
        assert_eq!(
            new_snake.slide(),
            Snake {
                cells: vec![Cell { x: 3, y: 0 }, Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }, Cell { x: 0, y: 0 }],
                direction: Direction::Right,
                eaten_apples: 0
            }
        );
    }
}

#[cfg(test)]
mod game_tests {
    use super::*;

    #[test]
    fn game_is_over_when_snake_hits_border() {
        let snake = Snake::new(
            vec![Cell { x: 2, y: 0 }, Cell { x: 1, y: 0 }, Cell { x: 0, y: 0 }],
            Direction::Right
        );
        let game = Game { snake, width: 3, height: 1, apples: Apples::create(3, 1) };

        assert_eq!(game.is_over(), false);
        assert_eq!(game.update(None).is_over(), true);
        assert_eq!(game.update(Some(Direction::Down)).is_over(), true);
    }

    #[test]
    fn game_is_over_when_snake_bites_itself() {
        let snake = Snake::new(
            vec![Cell { x: 0, y: 0 }, Cell { x: 0, y: 1 }, Cell { x: 1, y: 1 }, Cell { x: 1, y: 0 }, Cell { x: 0, y: 0 }],
            Direction::Right
        );
        let game = Game { snake, width: 100, height: 100, apples: Apples::create(3, 1) };

        assert_eq!(game.is_over(), true);
    }
}

#[cfg(test)]
mod apples_tests {
    use super::*;

    #[test]
    fn apples_grow_at_random_locations() {
        let seed = [
            1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5,
            1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2
        ];
        let rng = ChaChaRng::from_seed(seed);
        let mut apples = Apples {
            field_width: 20,
            field_height: 10,
            cells: vec![],
            growth_speed: 3,
            rng
        };

        assert_eq!(
            apples.grow().grow().grow().cells,
            vec![Cell { x: 7, y: 0 }]
        );
    }
}