package xpday.enigma

import org.junit.Ignore
import org.junit.Test

/**
 * User: dima
 * Date: 22/11/2011
 */
class EnigmaTest {
  @Test public void when_have_one_rotor_same_input_should_output_rotor_content() {
    new Enigma("DMTWSILRUYQNKFEJCAZBPGXOHV").with { enigma ->
      assert enigma.type("A" * 26) == "DMTWSILRUYQNKFEJCAZBPGXOHV"
      assert enigma.type("B" * 26) == "MTWSILRUYQNKFEJCAZBPGXOHVD"
      assert enigma.type("AB") == "DT"
    }
  }

  @Ignore // didn't finish
  @Test public void rotates_with_two_rotors() {
    new Enigma("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "ABCDEFGHIJKLMNOPQRSTUVWXYZ").with { enigma ->
      assert enigma.type("A" * 26) == "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      assert enigma.type("A" * 26) == "BCDEFGHIJKLMNOPQRSTUVWXYZA"
    }
  }

  @Test public void maps_first_rotor_two_second_rotor() {
    new Enigma("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "BCDEFGHIJKLMNOPQRSTUVWXYZA").with { enigma ->
      assert enigma.type("A" * 26) == "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    }
  }
}

class Enigma {
  List<String> rotors
  String alphabet

  Enigma(String... rotors) {
    this.alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    this.rotors = rotors
  }

  def type(String s) {
    s.toList().collect { typeChar(it) }.join("")
  }

  def typeChar(String c) {
    def n = alphabet.indexOf(c)
    def result = rotors[0][n]
    rotors[0] = rotate()
    result
  }

  def rotate() {
    rotors[0].toList().tail().join("") + rotors[0][0]
  }
}
