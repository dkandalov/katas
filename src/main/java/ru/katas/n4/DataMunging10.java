package ru.katas.n4;

import org.junit.Test;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static java.nio.charset.Charset.defaultCharset;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class DataMunging10 {
    @Test
    public void findDayWithMinimumTemperatureSpread() throws IOException {
        Path path = FileSystems.getDefault().getPath("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat");
        List<String> lines = Files.readAllLines(path, defaultCharset()).subList(8, 38);

        Row row = lines.stream()
                .map(it -> it.trim().split("\\s+"))
                .map(it -> new Row(it[0], asInt(it[1]), asInt(it[2])))
                .min((row1, row2) -> {
                    int diff1 = Math.abs(row1.value1 - row1.value2);
                    int diff2 = Math.abs(row2.value1 - row2.value2);
                    return Integer.compare(diff1, diff2);
                }).get();

        assertThat(row.key, equalTo("14"));
    }

    private static int asInt(String s) {
        return Integer.parseInt(s.replace("*", ""));
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
    }
}
