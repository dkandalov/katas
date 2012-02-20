package ru.katas.n13

import org.junit.Test
import ru.util.Pomodoro

/**
 * User: dima
 * Date: 20/02/2012
 */
@Pomodoro("2")
class LineCount0 {
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

  @Test public void test() {
    assert countLinesOf("") == 0
    assert countLinesOf("/** aaa */") == 0
    assert countLinesOf("/** \n */") == 0
    assert countLinesOf('''
System./*wait*/out./*for*/println/*it*/("Hello/*");
''') == 1
  }

  static countLinesOf(String source) { // (?s) - dot all !!!
    def oneLiner = /\/\//
    def startMulti = /\/\*/
    def endMulti = /\*\//

    source = source.trim()
    source = source.replaceAll(/[ \t\x0B\f\r]/, "")

    source = source.replaceAll(/(?s)${startMulti}.*?${endMulti}/) { it.contains("\n") ? "\n" : "" }
    source = source.replaceAll(/${oneLiner}.*?\n/, "\n")

    source = source.replaceAll(/(?s)\n\n+/, "\n")
    source = source.trim()

    source.empty ? 0 : source.count("\n") + 1
  }
}
