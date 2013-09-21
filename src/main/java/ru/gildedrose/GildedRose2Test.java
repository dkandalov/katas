package ru.gildedrose;

import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Test;

import java.io.*;

public class GildedRose2Test {
    @Test public void regression() throws Exception {
        String output = GildedRose2.gildedRose();
        String expected = readFile("src/main/java/ru/gildedrose/expected-output.txt");
        Assert.assertThat(output, CoreMatchers.equalTo(expected));
    }

    private static String readFile(String fileName) throws IOException {
        String result = "";
        InputStream inputStream = new BufferedInputStream(new FileInputStream(new File(fileName)));

        int i;
        while ((i = inputStream.read()) != -1) {
            result += (char) i;
        }
        return result;
    }
}
