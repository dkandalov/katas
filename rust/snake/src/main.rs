extern "C" {
    fn getpid() -> i32;
}

fn main() {
    let pid = unsafe { getpid() };
    println!("Hello from {}", pid);
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