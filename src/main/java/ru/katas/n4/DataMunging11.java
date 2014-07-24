package ru.katas.n4;

import org.junit.Test;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.stream.Stream;

import static java.nio.charset.Charset.defaultCharset;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

public class DataMunging11 {

    @Test public void findDayWithMinimumTemperatureSpread() throws IOException {
        Stream<String> lines = readLines("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat", 8, 38);
        Row dayRow = convertToRows(lines, 0, 1, 2).min(rowDiff()).get();

        assertThat(dayRow.key, equalTo("14"));
    }

    @Test public void findFootballTeamWithMinimumGoalDifference() throws IOException {
        Stream<String> lines = readLines("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat", 5, 26);
        Row teamRow = convertToRows(lines, 1, 6, 8).min(rowDiff()).get();

        assertThat(teamRow.key, equalTo("Aston_Villa"));
    }

    private static Stream<Row> convertToRows(Stream<String> lines, int keyIndex, int value1Index, int value2Index) {
        return lines
                .filter(it -> !it.contains("---"))
                .map(s -> s.trim().split("\\s+"))
                .map(it -> new Row(it[keyIndex], asInt(it[value1Index]), asInt(it[value2Index])));
    }

    private static Stream<String> readLines(String filePath, int fromLine, int toLine) throws IOException {
        Path path = FileSystems.getDefault().getPath(filePath);
        return Files.readAllLines(path, defaultCharset()).subList(fromLine, toLine).stream();
    }

    private static Comparator<Row> rowDiff() {
        return (row1, row2) -> Integer.compare(row1.diff, row2.diff);
    }

    private static int asInt(String s) {
        return Integer.parseInt(s.replace("*", ""));
    }

    private static class Row {
        private final String key;
        private final int value1;
        private final int value2;
        private final int diff;

        public Row(String key, int value1, int value2) {
            this.key = key;
            this.value1 = value1;
            this.value2 = value2;
            this.diff = Math.abs(value1 -  value2);
        }

        @Override public String toString() {
            return "Row{" +
                    "key='" + key + '\'' +
                    ", value1=" + value1 +
                    ", value2=" + value2 +
                    ", diff=" + diff +
                    '}';
        }
    }
}
