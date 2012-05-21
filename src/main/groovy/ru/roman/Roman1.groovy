package ru.roman

import org.junit.Test

/**
 * Done at coding dojo.
 */
class Roman1 {
  @Test void should_convert_1_to_I() {
//    assert [1, 5, 10, 50, 100, 500, 1000].collect { toRoman(it) } == ["I", "V", "X", "L", "C", "D", "M"]
    assert toRoman(1) == "I"
    assert toRoman(4) == "IV"
    assert toRoman(6) == "VI"
    assert toRoman(7) == "VII"
    assert toRoman(8) == "VIII"
    assert toRoman(9) == "IX"
    assert toRoman(11) == "XI"
    assert toRoman(22) == "XXII"
    assert toRoman(32) == "XXXII"
    assert toRoman(39) == "XXXIX"
    assert toRoman(40) == "XL"
    assert toRoman(42) == "XLII"
  }

  static String toRoman(int n) {
    def result = ""
    def romanNumbers = [1000: "M", 500: "D", 100: "C", 50: "L", 10: "X", 5: "V", 1: "I"]


    while (n > 0) {
      def keys = romanNumbers.keySet();
      def previousValue = ""
      for (def key: keys) {
        
//        if (n % key < )
        
        def product = n.intdiv(key)
        if (product > 3) {
          result += romanNumbers[key] + previousValue
          n %= key
        } else if (product > 0) {
          result += romanNumbers[key] * product
          n %= key
        }

        previousValue = romanNumbers[key]
      }
    }

    result
  }
}
