package katas.groovy.sudoku

import org.junit.Test

/**
 * User: dima
 * Date: 5/3/11
 */
class Sudoku0 { // TODO finish me!!
    static int X = Cell.NO_VALUE

    @Test public void cellBoxShouldVerifyThatItsContentIsCorrect() {
        def box = new Box([
                [2, 1, 3],
                [4, 5, 6],
                [7, 8, 9]
        ])
        assert box.correct
        assert box[0].value == 2

        box = new Box([
                [1, 2, 3],
                [4, 5, 6],
                [7, 8, 1]
        ])
        assert !box.correct
    }

    @Test public void boardShouldFillItsContentCorrectly() {
        def board = Board.create([
                [1, 2, 3], [1, 2, 3], [1, 2, 5],
                [1, X, X], [X, X, X], [X, X, X],
                [1, X, X], [X, X, X], [X, X, X],

                [1, 2, 3], [1, 2, 3], [1, 2, 3],
                [1, 2, 3], [1, 2, 3], [1, 2, 3],
                [1, 2, 3], [1, 2, 3], [1, 2, 3],

                [1, 2, 3], [1, 2, 3], [1, 2, 9],
                [1, 2, 3], [1, 2, 3], [4, 2, 3],
                [1, 2, 3], [1, 2, 3], [5, 2, 8]
        ])
        assert board.rows[0] == new Row([[1, 2, 3], [1, 2, 3], [1, 2, 5]])
        assert board.columns[0] == new Column([[1, 1, 1], [1, 1, 1], [1, 1, 1]])
        assert board.boxes[0] == new Box([[1, 2, 3], [1, X, X], [1, X, X]])

        assert board.rows[0][0].row == new Row([[1, 2, 3], [1, 2, 3], [1, 2, 5]])
        assert board.rows[0][0].column == new Column([[1, 1, 1], [1, 1, 1], [1, 1, 1]])
        assert board.rows[0][0].box == new Box([[1, 2, 3], [1, X, X], [1, X, X]])

        assert board.rows[8] == new Row([[1, 2, 3], [1, 2, 3], [5, 2, 8]])
        assert board.columns[8] == new Column([[5, X, X], [3, 3, 3], [9, 3, 8]])
        assert board.boxes[8] == new Box([[1, 2, 9], [4, 2, 3], [5, 2, 8]])

        assert !board.correct
    }

    @Test public void solverShouldFindNextValueForRow() {
        def board = Board.create([
                [1, X, X], [X, X, X], [X, X, X],
                [X, 2, X], [X, X, X], [X, X, X],
                [X, X, X], [X, X, X], [2, X, X],

                [3, X, X], [5, X, X], [7, X, X],
                [X, X, X], [X, 2, X], [X, X, X],
                [X, X, X], [X, X, X], [X, X, X],

                [4, X, X], [6, X, 2], [8, X, X],
                [X, X, X], [X, X, X], [X, X, X],
                [X, X, X], [X, X, X], [X, X, X],
        ])
        def solver = new Solver(board)
        solver.doStep()

        assert board == Board.create([
                [1, X, X], [2, X, X], [X, X, X],
                [X, 2, X], [X, X, X], [X, X, X],
                [X, X, X], [X, X, X], [2, X, X],

                [3, X, X], [5, X, X], [7, X, X],
                [X, X, X], [X, 2, X], [X, X, X],
                [X, X, X], [X, X, X], [X, X, X],

                [4, X, X], [6, X, 2], [8, X, X],
                [X, X, X], [X, X, X], [X, X, X],
                [X, X, X], [X, X, X], [X, X, X],
        ])
    }

    private static class Solver {
        Board board

        Solver(Board board) {
            this.board = board
        }

        def doStep() {
            for (Row row : board.rows) {
                def missingValues = (1..9).asList() - row.cells.collect {it.value}
                for (int missingValue : missingValues) {
                    def boxes = boxesForRow(row)
                    boxes = boxesWithout(missingValue, boxes)
                    def emptyCells = emptyCells(intersect(boxes, row))
                    def potentialCells = emptyCells.findAll {!it.column.contains(new Cell(missingValue))}

                    if (potentialCells.size() == 1) {
                        potentialCells[0].value = missingValue
                        return // didn't expect that I'll need to return, had to use iteration instead of clusures
                    }
                }
            }
        }

        Collection<Box> boxesForRow(Row row) {
            [0, 3, 6].collect {row[it].box}
        }

        Collection<Box> boxesWithout(int missingValue, Collection<Box> boxes) {
            def boxesToIgnore = boxes.findAll {box -> box.contains(new Cell(missingValue))} // used collect instead of findAll
            if (boxesToIgnore.size() >= 3)
                throw new IllegalStateException("Value ${missingValue} shouldn't be missing because it's present in all boxes")
            boxes - boxesToIgnore
        }

        Collection<Cell> intersect(Collection<Box> boxes, Row row) {
            row.findAll {rowCell -> boxes.contains(rowCell.box)} // used wrong logic
        }

        def emptyCells(Collection<Cell> cells) {
            cells.findAll {it.value == Cell.NO_VALUE} // used collect instead of findAll
        }
    }

    private static class Board {
        List<Row> rows = []
        List<Column> columns = []
        List<Box> boxes = []

        static Board create(List<List<Integer>> values) {
            def board = new Board()
            9.times {
                board.rows << new Row()
                board.columns << new Column()
                board.boxes << new Box()
            }

            values.flatten().eachWithIndex { int value, i ->
                def row = board.rows[i.intdiv(9)]
                def column = board.columns[i % 9]

                int boxColumn = (i % 9).intdiv(3)
                int boxRow = i.intdiv(9).intdiv(3)
                def box = board.boxes[(boxRow * 3) + boxColumn]

                def cell = new Cell(value)
                cell.row = row
                cell.column = column
                cell.box = box

                row[i % 9] = cell
                column[i.intdiv(9)] = cell
                int columnInBox = i % 3
                int rowInBox = i.intdiv(9) % 3
                box[(rowInBox * 3) + columnInBox] = cell
            }

            board
        }

        boolean isCorrect() {
            [rows, columns, boxes].every {
                it.every { cellSequence ->
                    cellSequence.correct
                }
            }
        }

        @Override String toString() {
            rows.collect {it.toString() + "\n"}
        }

        @Override boolean equals(o) {
            if (this.is(o)) return true;
            if (getClass() != o.class) return false;

            Board board = (Board) o;

            if (boxes != board.boxes) return false;
            if (columns != board.columns) return false;
            if (rows != board.rows) return false;

            return true;
        }

        @Override int hashCode() {
            int result;
            result = (rows != null ? rows.hashCode() : 0);
            result = 31 * result + (columns != null ? columns.hashCode() : 0);
            result = 31 * result + (boxes != null ? boxes.hashCode() : 0);
            return result;
        }
    }

    private static class Row extends CellSequence {
        Row() {}

        Row(List<List<Integer>> values) { super(values) }

        @Override
        String toString() {
            cells[0..2].collect {it.toString()} + " " +
            cells[3..5].collect {it.toString()} + " " +
            cells[6..8].collect {it.toString()}
        }
    }
    private static class Column extends CellSequence {
        Column() {}

        Column(List<List<Integer>> values) { super(values) }
    }
    private static class Box extends CellSequence {
        Box() {}

        Box(List<List<Integer>> values) { super(values) }
    }

    private static class CellSequence {
//        @Delegate List<Cell> cells = [] // TODO

        CellSequence() {
            9.times {cells << new Cell(Cell.NO_VALUE)}
        }

        CellSequence(List<List<Integer>> values) {
            cells = values.flatten().collect {new Cell((int) it)}
        }

        boolean isCorrect() {
            cells.collect {it.value}.asList().sort().equals((1..9))
        }
    }

    private static class Cell {
        static int NO_VALUE = -1

        int value
        Row row
        Column column
        Box box

        Cell(int value) {
            this.value = value
        }

        @Override public String toString() {
            return (value == NO_VALUE ? "X" : value)
        }

        String toFullString() {
            null
        }

        boolean equals(o) {
            if (this.is(o)) return true;
            if (getClass() != o.class) return false;

            Cell cell = (Cell) o;

            if (value != cell.value) return false;

            return true;
        }

        int hashCode() {
            return value;
        }
    }
}
