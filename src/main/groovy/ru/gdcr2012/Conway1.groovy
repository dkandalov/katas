package ru.gdcr2012

import org.junit.Test

class Conway1 {
  static int ONE_NEIGHBOUR = 1
  static int TWO_NEIGHBOURS = 2
  static int THREE_NEIGHBOURS = 3
  static int FOUR_NEIGHBOURS = 4

  static boolean LIVE = true
  static boolean DEAD = false

  static NO_CELLS = []

  @Test void live_cell_should_die_when_has_one_live_neighbour() {
    Cell cell = new Cell(ONE_NEIGHBOUR, LIVE)
    cell.evolve()
    assert !cell.isAlive()
  }

  @Test void live_cell_should_die_when_it_has_more_than_three_neighbours() {
    Cell cell = new Cell(FOUR_NEIGHBOURS, LIVE)
    cell.evolve()
    assert !cell.isAlive()
  }

  @Test void live_cell_should_survive_when_it_has_two_neighbours() {
    Cell cell = new Cell(TWO_NEIGHBOURS, LIVE)
    cell.evolve()
    assert cell.isAlive()
  }

  @Test void live_cell_should_survive_when_it_has_three_neighbours() {
    Cell cell = new Cell(THREE_NEIGHBOURS, LIVE)
    cell.evolve()
    assert cell.isAlive()
  }

  @Test void dead_cell_should_become_alive_when_it_has_exactly_three_neighbours() {
    Cell cell = new Cell(THREE_NEIGHBOURS, DEAD)
    cell.evolve()
    assert cell.isAlive()
  }

  @Test void empty_board_should_evolve_into_empty_board() {
    def board = new Board(NO_CELLS)
    assert board.evolve().cells == NO_CELLS
  }


  private class Board {
    def cells

    Board(cells) {
      this.cells = cells
    }

    def evolve() {
      this
    }
  }

  private class Cell {
    boolean isAlive
    int amountOfNeighbours

    Cell(int amountOfNeighbours, boolean isAlive) {
      this.isAlive = isAlive
      this.amountOfNeighbours = amountOfNeighbours
    }

    def evolve() {
    }

    def isAlive() {
      amountOfNeighbours == 2 || amountOfNeighbours == 3
    }
  }
}
