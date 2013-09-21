package ru.gildedrose

import org.junit.Test

import static org.hamcrest.CoreMatchers.equalTo
import static org.junit.Assert.assertThat
import static ru.gildedrose.GlidedRose1.gildedRose

class GildedRose1Test {
  @Test void regression() {
    def output = gildedRose()
    def expected = new File("src/main/java/ru/gildedrose/expected-output.txt").readLines().join("\n")
    assertThat(output.trim(), equalTo(expected))
  }
}
