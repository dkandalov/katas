require "test/unit"

class RomanNumbers < Test::Unit::TestCase
  def test_should_convert_arab_numbers_to_roman_numbers
    assert_equal as_roman(1), "I"
    assert_equal as_roman(2), "II"
    assert_equal as_roman(3), "III"
    assert_equal as_roman(4), "IV"
    assert_equal as_roman(5), "V"

    assert_equal as_roman(6), "VI"
    assert_equal as_roman(7), "VII"
    assert_equal as_roman(8), "VIII"
    assert_equal as_roman(9), "IX"
    assert_equal as_roman(10), "X"

    assert_equal as_roman(11), "XI"
    assert_equal as_roman(12), "XII"
    assert_equal as_roman(13), "XIII"
    assert_equal as_roman(14), "XIV"
    assert_equal as_roman(15), "XV"
    assert_equal as_roman(16), "XVI"
    assert_equal as_roman(17), "XVII"
    assert_equal as_roman(18), "XVIII"
    assert_equal as_roman(19), "XIX"
    assert_equal as_roman(20), "XX"
    assert_equal as_roman(21), "XXI"
    assert_equal as_roman(22), "XXII"
    assert_equal as_roman(28), "XXVIII"
    assert_equal as_roman(29), "XXIX"
    assert_equal as_roman(30), "XXX"
    assert_equal as_roman(31), "XXXI"
    assert_equal as_roman(35), "XXXV"
    assert_equal as_roman(39), "XXXIX"

    assert_equal as_roman(40), "XL"
    assert_equal as_roman(41), "XLI"
    assert_equal as_roman(42), "XLII"
    assert_equal as_roman(43), "XLIII"
    assert_equal as_roman(44), "XLIV"
    assert_equal as_roman(45), "XLV"
    assert_equal as_roman(46), "XLVI"
    assert_equal as_roman(47), "XLVII"
    assert_equal as_roman(48), "XLVIII"
    assert_equal as_roman(49), "XLIX"
    assert_equal as_roman(50), "L"

    assert_equal as_roman(51), "LI"
    assert_equal as_roman(55), "LV"
    assert_equal as_roman(59), "LIX"
    assert_equal as_roman(60), "LX"
    assert_equal as_roman(61), "LXI"
    assert_equal as_roman(65), "LXV"
    assert_equal as_roman(69), "LXIX"
    assert_equal as_roman(70), "LXX"
    assert_equal as_roman(80), "LXXX"
    assert_equal as_roman(81), "LXXXI"
    assert_equal as_roman(85), "LXXXV"
    assert_equal as_roman(88), "LXXXVIII"
    assert_equal as_roman(89), "LXXXIX"
    assert_equal as_roman(90), "XC"
    assert_equal as_roman(91), "XCI"
    assert_equal as_roman(95), "XCV"
    assert_equal as_roman(99), "XCIX"
    assert_equal as_roman(100), "C"

    assert_equal as_roman(101), "CI"
  end

  def as_roman n
    if n < 4
      "I" * n
    elsif n == 4
      "IV"
    elsif n >= 5 and n < 9
      "V" + as_roman(n - 5)
    elsif n == 9
      "IX"
    elsif n >= 10 and n < 40
      "X" + as_roman(n - 10)
    elsif n >= 40 and n < 50
      "XL" + as_roman(n - 40)
    elsif n >= 50 and n < 90
      "L" + as_roman(n - 50)
    elsif n >= 90 and n < 100
      "XC" + as_roman(n - 90)
    elsif n >= 100
      "C" + as_roman(n - 100)
    end
  end
end