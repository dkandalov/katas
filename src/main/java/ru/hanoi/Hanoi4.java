package ru.hanoi;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class Hanoi4 {
    @Test public void findMovesToSolveTowersOfHanoiProblem() {
        assertThat(solveHanoi(1), equalTo(asList(move(0, -1))));
        assertThat(solveHanoi(2), equalTo(asList(move(0, 1), move(1, -1), move(0, 1))));
        assertThat(solveHanoi(3), equalTo(asList(
                move(0, -1), move(1, 1), move(0, -1),
                move(2, -1),
                move(0, -1), move(1, 1), move(0, -1)
        )));
    }

    private List<Move> solveHanoi(int i) {
        return solveHanoi(i, -1);
    }

    private List<Move> solveHanoi(int i, int direction) {
        if (i == 0) return Collections.emptyList();
        else {
            List<Move> result = new ArrayList<>();
            result.addAll(solveHanoi(i - 1, -direction));
            result.add(move(i - 1, direction));
            result.addAll(solveHanoi(i - 1, -direction));
            return result;
        }
    }

    private static Move move(int plate, int shift) {
        return new Move(plate, shift);
    }

    private static class Move {
        final int plate;
        final int shift;

        private Move(int plate, int shift) {
            this.plate = plate;
            this.shift = shift;
        }

        @Override public String toString() {
            return "(" + plate + "," + shift + ")";
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Move move = (Move) o;

            if (plate != move.plate) return false;
            if (shift != move.shift) return false;

            return true;
        }

        @Override
        public int hashCode() {
            int result = plate;
            result = 31 * result + shift;
            return result;
        }
    }
}
