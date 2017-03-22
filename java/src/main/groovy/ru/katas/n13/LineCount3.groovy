package ru.katas.n13

import org.junit.Test

class LineCount3 {
  @Test void aaa() {
    assert countLines("") == 0
    assert countLines("""
""") == 0
    assert countLines("""

  int i = 0;
""") == 1
    assert countLines("""
  int i = 0;
""") == 1
    assert countLines("""
// int i = 0;
""") == 0
    assert countLines("""
 int j = 0; // int i = 0;
""") == 1
    assert countLines("""
/* int i = 0; */
""") == 0
    assert countLines("""
/* int i = 0;
   int j = 0; */
""") == 0
    assert countLines("""
/* int i = 0;
   int j = 0; */ double d = 0;
""") == 1
  }

  @Test void examples() {
    assert countLines(new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n13/java_source1.txt").readLines().join("\n")) == 3
    assert countLines(new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n13/java_source2.txt").readLines().join("\n")) == 5
  }

  private static int countLines(String lines) {
    lines = removeMultilineCommentsFrom(lines)
    lines = removeSingleLineCommentsFrom(lines)
    lines = removeLinesWithOnlyWhitespaces(lines)
    lines = collapseSequentialNewLines(lines)
    lines = lines.trim()

    println(lines)
    if (!lines.empty) lines.count("\n") + 1
    else lines.count("\n")
  }

  public static String removeLinesWithOnlyWhitespaces(String lines) {
    lines.replaceAll(/\s+\n/, "\n")
  }

  public static String collapseSequentialNewLines(String lines) {
    lines.replaceAll(/\n+/, "\n")
  }

  public static String removeSingleLineCommentsFrom(String lines) {
    lines.replaceAll(/\/\/.*/, "")
  }

  public static String removeMultilineCommentsFrom(lines) {
    lines.replaceAll(/(?s)\/\*.*?\*\//, "")
  }
}
