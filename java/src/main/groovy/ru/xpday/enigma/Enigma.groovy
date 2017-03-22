package ru.xpday.enigma

import org.junit.Test

/**
 * See http://en.wikipedia.org/wiki/Enigma_rotor_details
 *
 * User: dima
 * Date: 22/11/2011
 */
class EnigmaTest {
  @Test public void WHEN_has_one_rotor_THEN_same_input_should_output_rotor_content() {
    new Enigma("DMTWSILRUYQNKFEJCAZBPGXOHV").with {
      assert it.type("A" * 26) == "DMTWSILRUYQNKFEJCAZBPGXOHV"
    }
    new Enigma("DMTWSILRUYQNKFEJCAZBPGXOHV").with {
      assert it.type("B" * 26) == "MTWSILRUYQNKFEJCAZBPGXOHVD"
    }
  }

  @Test public void WHEN_has_one_rotor_THEN_it_should_rotate_once_for_each_typed_character() {
    new Enigma("DMTWSILRUYQNKFEJCAZBPGXOHV").with {
      assert it.type("ABC") == "DTS"
    }
  }

  @Test public void WHEN_has_two_rotors_THEN_shifts_second_rotor_after_full_cycle_of_first_rotor() {
    new Enigma("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "ABCDEFGHIJKLMNOPQRSTUVWXYZ").with {
      assert it.type("A" * 26) == "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      assert it.type("A" * 26) == "BCDEFGHIJKLMNOPQRSTUVWXYZA"
    }
  }

  @Test public void WHEN_has_two_rotors_THEN_maps_output_from_first_rotor_to_second_rotor() {
    new Enigma("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "BCDEFGHIJKLMNOPQRSTUVWXYZA").with {
      assert it.type("A" * 26) == "BCDEFGHIJKLMNOPQRSTUVWXYZA"
    }
    new Enigma("BCDEFGHIJKLMNOPQRSTUVWXYZA", "CDEFGHIJKLMNOPQRSTUVWXYZAB").with {
      assert it.type("A" * 26) == "DEFGHIJKLMNOPQRSTUVWXYZABC"
    }
  }

  @Test public void WHEN_has_three_rotors_THEN_maps_output_from_first_to_second_to_third_rotor() {
    new Enigma("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "ABCDEFGHIJKLMNOPQRSTUVWXYZ").with {
      assert it.type("A" * 26) == "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    }
    new Enigma("BCDEFGHIJKLMNOPQRSTUVWXYZA", "CDEFGHIJKLMNOPQRSTUVWXYZAB", "DEFGHIJKLMNOPQRSTUVWXYZABC").with {
      assert it.type("A" * 26) == "GHIJKLMNOPQRSTUVWXYZABCDEF"
    }
  }
}

class Enigma {
  private List<String> rotors
  private final String alphabet
  private int rotationCount

  Enigma(String... rotors) {
    this.alphabet = ("A".."Z").join("")
    this.rotors = rotors
  }

  def type(String s) {
    s.toList().collect { typeChar(it) }.join("")
  }

  private def typeChar(String c) {
    def result = c
    rotors.each { rotor ->
      def n = alphabet.indexOf(result)
      result = rotor[n]
    }

    rotationCount++
    rotors.eachWithIndex { rotor, i ->
      if (i == 0 || rotationCount % (i * alphabet.size()) == 0) {
        rotors[i] = rotate(rotor)
      }
    }

    result
  }

  private def rotate(String rotor) {
    rotor.toList().tail().join("") + rotor[0]
  }
}
