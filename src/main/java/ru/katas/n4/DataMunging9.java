package ru.katas.n4;

import org.junit.Test;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

import static java.lang.Integer.compare;
import static java.lang.Integer.parseInt;
import static java.lang.Math.abs;
import static java.nio.charset.Charset.defaultCharset;
import static java.nio.file.Files.readAllLines;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class DataMunging9 {

    @Test public void findDayWithMinimumTemperatureSpread() throws IOException {
        List<String> lines = readLines("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat", 8, 38);
        Row minTemperatureSpread = parseRows(lines, 0, 1, 2).min(valueDiff()).get();

        assertThat(minTemperatureSpread.key, equalTo("14"));
    }

    @Test public void findFootballTeamWithMinimumGoalDifference() throws IOException {
        List<String> lines = readLines("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat", 5, 26);
        Row rows = parseRows(lines, 1, 6, 8).min(valueDiff()).get();

        assertThat(rows.key, equalTo("Aston_Villa"));
    }

    private static List<String> readLines(String filePath, int fromLine, int toLine) throws IOException {
        Path path = FileSystems.getDefault().getPath(filePath);
        return readAllLines(path, defaultCharset()).subList(fromLine, toLine);
    }

    private static Stream<Row> parseRows(List<String> lines, int keyColumn, int value1Column, int value2Column) {
        return lines.stream()
                .filter(it -> !it.contains("---"))
                .map(it -> it.trim().split("\\s+"))
                .map(it -> new Row(it[keyColumn], asInt(it[value1Column]), asInt(it[value2Column])));
    }

    private static int asInt(String s) {
        return parseInt(s.replace("*", ""));
    }

    private static Comparator<Row> valueDiff() {
        return (row1, row2) -> {
            int diff1 = abs(row1.value1 - row1.value2);
            int diff2 = abs(row2.value1 - row2.value2);
            return compare(diff1, diff2);
        };
    }

    private static class Row {
        final String key;
        final int value1;
        final int value2;

        private Row(String key, int value1, int value2) {
            this.key = key;
            this.value1 = value1;
            this.value2 = value2;
        }

        @Override public String toString() {
            return "Row{key='" + key + '\'' + ", value1=" + value1 + ", value2=" + value2 + '}';
        }
    }
}
