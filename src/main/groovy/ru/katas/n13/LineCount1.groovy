package ru.katas.n13

import org.junit.Test
import katas.java.util.Pomodoro

/**
 * User: dima
 * Date: 22/02/2012
 */
@Pomodoro("1")
class LineCount1 {
  @Test public void file1() {
    def source = """
// This file contains 3 lines of code
public interface Dave {
   /**
    * count the number of lines in a file
    */
   int countLines(File inFile); // not the real signature!
}
"""
    assert countLinesOf(source) == 3
  }

  @Test public void file2() {
    def source = """
/*****
* This is a test program with 5 lines of code
*  \\/* no nesting allowed!
//*****//***/// Slightly pathological comment ending...

public class Hello {
  public static final void main(String [] args) { // gotta love Java
    // Say hello
    System./*wait*/out./*for*/println/*it*/("Hello/*");
  }

}
"""
    assert countLinesOf(source) == 5
  }

  static countLinesOf(String source) {
    source = source.trim()

    def multiLineStart = /\/\*\*/
    def multiLineEnd = /\*\//
    def oneLineStart = /\/\//
    def whiteSpaces = /[ \t\x0B\f]*/
    source = source.replaceAll(/${whiteSpaces}/, "")
    source = source.replaceAll(/(?ms)${multiLineStart}.*?${multiLineEnd}/) { it.contains("\n") ? "\n" : "" }
    source = source.replaceAll(/${oneLineStart}.*?\n/, "\n")
    source = source.trim().replaceAll(/\n{2,}+/, "\n")

    println source
    if (source.empty) 0
    else if (source.count("\n") == 0) 1
    else source.count("\n") + 1
  }
}
