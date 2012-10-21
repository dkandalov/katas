package ru.game_of_life

import org.junit.Test

/**
 * User: dima
 * Date: 21/10/2012
 */
class GOL0 {
  @Test void withoutCellsNothingHappens() {
    assert nextRound(asField("""
---
---
---
""")) == asField("""
---
---
---
""")
  }

  @Test void oneLonelyCellDies() {
    assert nextRound(asField("""
---
-0-
---
""")) == asField("""
---
---
---
""")
  }

  @Test void twoNeighbourCellsDoNotDie() {
    assert nextRound(asField("""
---
00-
---
""")) == asField("""
---
00-
---
""")
  }

  @Test void threeCellsCreateNewOne() {
    assert nextRound(asField("""
00-
0--
---
"""))[1][1] == CELL
  }

  @Test void cellWithFourNeighboursDiesOfOverpopulation() {
    assert nextRound(asField("""
000
00-
---
"""))[1][1] == EMPTY
  }

  @Test void attemptToCreateAGlider() {
    def field = asField("""
-0-----
000----
-00----
-------
-------
-------
-------
""")

    for (round in (0..20)) {
      println(field.collect { row -> row.join("") }.join("\n"))
      println("===========")
      field = nextRound(field)
    }
  }

  private static NONE = " "
  private static EMPTY = "-"
  private static CELL = "0"

  private static nextRound(List field, List newField = (0..<field.size()).collect{ [NONE, NONE, NONE] }) {
    (0..<field.size()).collect { row ->
      (0..<field.size()).collect { col ->
        if (isLonelyCell(row, col, field)) newField[row][col] = EMPTY
        else if (isEmptySpaceSurroundedByCells(row, col, field)) newField[row][col] = CELL
        else if (isOverpopulatedCell(row, col, field)) newField[row][col] = EMPTY
        else newField[row][col] = field[row][col]
      }
    }
    newField
  }

  private static boolean isOverpopulatedCell(row, col, field) {
    return cellAt(row, col, field) == CELL && amountOfNeighbourCells(row, col, field) >=4
  }

  private static boolean isEmptySpaceSurroundedByCells(row, col, field) {
    return cellAt(row, col, field) == EMPTY && amountOfNeighbourCells(row, col, field) >= 3
  }

  private static boolean isLonelyCell(row, col, field) {
    return cellAt(row, col, field) == CELL && amountOfNeighbourCells(row, col, field) == 0
  }

  private static int amountOfNeighbourCells(row, col, field) {
    int amountOfNeighbours =
      [[row - 1, col], [row + 1, col], [row, col - 1], [row, col + 1],
       [row - 1, col - 1], [row - 1, col + 1], [row + 1, col + 1], [row + 1, col - 1]].inject(0) { acc, position ->
      cellAt(position[0], position[1], field) == CELL ? acc + 1 : acc
    }
    amountOfNeighbours
  }

  @Test public void shouldCellsWrappingAroundField() {
    def field = asField("""
0-0
---
--0
""")
    assert (0..2).collect{ cellAt(0, it, field) } == [CELL, EMPTY, CELL]
    assert (3..5).collect{ cellAt(0, it, field) } == [CELL, EMPTY, CELL]
    assert (-3..-1).collect{ cellAt(0, it, field) } == [CELL, EMPTY, CELL]

    assert (0..2).collect{ cellAt(it, 2, field) } == [CELL, EMPTY, CELL]
    assert (3..5).collect{ cellAt(it, 2, field) } == [CELL, EMPTY, CELL]
    assert (-3..-1).collect{ cellAt(it, 2, field) } == [CELL, EMPTY, CELL]
  }

  private static def cellAt(row, col, field) {
    field[(row + field.size()) % field.size()][(col + field.size()) % field.size()]
  }

  private static List asField(String s) {
    s.trim().split("\n").collect{ row -> row.toList() }
  }
}
