package ru.gildedrose
import org.junit.Test

import static org.hamcrest.CoreMatchers.equalTo
import static org.junit.Assert.assertThat

class GildedRose1Test {
  @Test void regression() {
    def output = GildedRose2.gildedRose()
    def expected = new File("src/main/java/ru/gildedrose/expected-output.txt").readLines().join("\n")
    assertThat(output.trim(), equalTo(expected))
  }
}
