package ru.katas.n4;

import org.junit.Test;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
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
        Path path = FileSystems.getDefault().getPath("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat");
        List<String> lines = readAllLines(path, defaultCharset()).subList(8, 38);
        assertThat(lines.size(), equalTo(30));

        Row minTemperatureSpread = lines.stream()
                .map(it -> it.trim().split("\\s+"))
                .map(it -> new Row(it[0], asInt(it[1]), asInt(it[2])))
                .min((row1, row2) -> compare(abs(row1.value1 - row1.value2), abs(row2.value1 - row2.value2)))
                .get();

        assertThat(minTemperatureSpread.key, equalTo("14"));
    }

    @Test public void findFootballTeamWithMinimumGoalDifference() throws IOException {
        Path path = FileSystems.getDefault().getPath("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat");
        Stream<String> lines = Files.readAllLines(path, defaultCharset()).subList(5, 26).stream();

        lines = lines.filter(it -> !it.contains("---"));

        Row rows = lines
                .map(it -> it.trim().split("\\s+"))
                .map(it -> new Row(it[1], asInt(it[6]), asInt(it[8])))
                .min((row1, row2) -> {
                    int diff1 = abs(row1.value1 - row1.value2);
                    int diff2 = abs(row2.value1 - row2.value2);
                    return compare(diff1, diff2);
                })
                .get();

        assertThat(rows.key, equalTo("Aston_Villa"));
    }
    
    private static int asInt(String s) {
        return parseInt(s.replace("*", ""));
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
            return "Row{" +
                    "key='" + key + '\'' +
                    ", value1=" + value1 +
                    ", value2=" + value2 +
                    '}';
        }
    }
}
