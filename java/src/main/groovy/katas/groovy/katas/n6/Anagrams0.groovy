package katas.groovy.katas.n6

import org.junit.Test

import static org.hamcrest.core.IsEqual.equalTo
import static org.junit.Assert.assertThat

/**
 * User: dima
 * Date: 18/11/2012
 */
class Anagrams0 {
  @Test void wordCount() {
    def lines = readWords("/usr/share/dict/words")
    assertThat(lines.size(), equalTo(234371))
  }

  @Test void groupingByCharacters() {
    def grouped = groupByChars(readWords("/usr/share/dict/words"))
    assertThat(grouped["act"], equalTo(["act", "cat"]))
    assertThat(grouped.size(), equalTo(14362))
  }

  @Test void findingLongestWordWhichIsAnagram() {
    def grouped = groupByChars(readWords("/usr/share/dict/words"))

    def longestWord = grouped.max { it.key.size() }.value
    assert longestWord == ["cholecystoduodenostomy", "duodenocholecystostomy"]
  }

  @Test void findingLongestAnagram() {
    def grouped = groupByChars(readWords("/usr/share/dict/words"))

    def longestAnagram = grouped.values().max{ it.size() }
    assert longestAnagram == ["angor", "argon", "goran", "grano", "groan", "nagor", "orang", "organ", "rogan", "ronga"]
  }

  private static Map groupByChars(Collection<String> lines) {
    def grouped = lines.groupBy { it.toList().sort().join("") }
    grouped.values().removeAll{ it.size() < 2 }
    grouped
  }

  private static Collection<String> readWords(String fileName) {
    def lines = new TreeSet()
    new File(fileName).eachLine { lines << it.toLowerCase() }
    lines
  }
}
