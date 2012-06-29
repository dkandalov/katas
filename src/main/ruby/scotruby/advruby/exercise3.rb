require 'test/unit'
require_relative 'string-extensions'

class TestSanitize < Test::Unit::TestCase

  def test_regular_characters
    ok_string = "qwertyuiop asdfghjkl zxcvbnm 1234567890"
    assert_equal ok_string, ok_string.sanitize
  end

  def test_remove_other_characters
    assert_equal "dave thomas", "&^%d[[av]e !!!th\aom@@a^s.....".sanitize
  end
end
