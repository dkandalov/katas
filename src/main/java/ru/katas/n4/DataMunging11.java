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
        Path path = FileSystems.getDefault().getPath("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat");
        Stream<String> lines = Files.readAllLines(path, defaultCharset()).subList(8, 38).stream();
        Stream<Row> dayRows = lines
                .filter(it -> !it.contains("---"))
                .map(s -> s.trim().split("\\s+"))
                .map(it -> new Row(it[0], asInt(it[1]), asInt(it[2])));
        Row dayRow = dayRows.min(rowDiff()).get();

        assertThat(dayRow.key, equalTo("14"));
    }

    @Test public void findFootballTeamWithMinimumGoalDifference() throws IOException {
        Path path = FileSystems.getDefault().getPath("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat");
        Stream<String> lines = Files.readAllLines(path, defaultCharset()).subList(5, 26).stream();
        Stream<Row> teamRows = lines
                .filter(it -> !it.contains("---"))
                .map(s -> s.trim().split("\\s+"))
                .map(it -> new Row(it[1], asInt(it[6]), asInt(it[8])));

        Row teamRow = teamRows.min(rowDiff()).get();
        assertThat(teamRow.key, equalTo("Aston_Villa"));
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
