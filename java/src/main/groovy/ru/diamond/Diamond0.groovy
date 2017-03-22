package ru.diamond

import org.junit.Test

import static org.hamcrest.CoreMatchers.equalTo
import static org.junit.Assert.assertThat

class Diamond0 {
  @Test void "create diamond string"() {
    assertThat(diamond('A', 'C'), equalTo("""
      |--A--
      |-B-B-
      |C---C
      |-B-B-
      |--A--
    """.stripMargin("|").trim()))

    assertThat(diamond('A', 'D'), equalTo("""
      |---A---
      |--B-B--
      |-C---C-
      |D-----D
      |-C---C-
      |--B-B--
      |---A---
    """.stripMargin("|").trim()))
  }

  private static String diamond(from, to) {
    from = from[0]
    to = to[0]
    def letters = (from..to).toList()

    def result = []
    letters.eachWithIndex { letter, i ->
      def sidePadding = ""
      (letters.size() - i - 1).times{ sidePadding += "-" }

      def midPadding = ""
      (i * 2 - 1).times { midPadding += "-" }

      if (midPadding.empty) result.add(sidePadding + letter.toString() + sidePadding)
      else result.add(sidePadding + letter.toString() + midPadding + letter.toString() + sidePadding)
    }
    (result + result.reverse().drop(1)).join("\n")
  }
}
