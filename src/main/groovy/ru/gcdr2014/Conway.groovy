package ru.gcdr2014

import groovy.transform.Immutable

// Yes, this is cheating and not deleting the code, sorry.
// Last session results with "No TDD" constraint, a bit modified by guys who inherited the code.
class Conway {
  static void main(String[] args) {
    def board = new Board(
            new Cell(1, 2),
            new Cell(2, 1),
            new Cell(2, 3)
    )
    println(board)
    println("===========")
    println(board.nextGeneration())
    println("===========")
    println(board.nextGeneration().nextGeneration())
  }

  static class Board {
    final List<Cell> allCells

    Board(Cell... allCells) {
      this.allCells = allCells.toList()
    }

    Board(Collection<Cell> allCells) {
      this.allCells = allCells
    }

    Board nextGeneration() {
      if (allCells.empty) return this

      def left = allCells.min{ it.x }.x
      def up = allCells.min{ it.y }.y
      def right = allCells.max{ it.x }.x
      def down = allCells.max{ it.y }.y

      final List<Cell> candidates = new ArrayList<Cell>()
      for (Cell cell in allCells) {
        candidates.addAll(cell.neighbours())
      }
      candidates.addAll(allCells)

      def newCells = []
      (up-1..down+1).each { int row ->
        (left-1..right+1).each { int column ->
          def cell = candidates.find { it == new Cell(column, row) }
          if (cell == null) return

          int amountOfNeighbours = candidates.count{ cell.neighbours().contains(it) }
          if (amountOfNeighbours == 2 || amountOfNeighbours == 3) {
            newCells.add(new Cell(column, row))
          }
        }
      }
      new Board(newCells)
    }

    @Override String toString() {
      if (allCells.empty) return ""
      def left = allCells.min{ it.x }.x
      def up = allCells.min{ it.y }.y
      def right = allCells.max{ it.x }.x
      def down = allCells.max{ it.y }.y

      def result = ""
      (up-1..down+1).each { int row ->
        (left-1..right+1).each { int column ->
          if (allCells.contains(new Cell(column, row))) result += "x"
          else result += "-"
        }
        result += "\n"
      }
      return result
    }
  }

  @Immutable
  static class Cell {
    int x
    int y

    def neighbours() {
      def shifts = [
              [x: -1, y: 0], // left
              [x: -1, y: -1], // left-top
              [x: 0, y: -1], // top
              [x: 1, y: -1], // top-right
              [x: 1, y: 0], // right
              [x: 1, y: 1], // right-down
              [x: 0, y: 1], // down
              [x: -1, y: 1], // down-left
      ]
      shifts.collect { shift ->
        new Cell(x + shift.x, y + shift.y)
      }
    }
  }
}

