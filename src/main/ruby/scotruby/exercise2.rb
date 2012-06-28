class SomeClass
  @attr_writers = []

  def self.my_attr_writer *names
    names.each do |name|
       attr_writer name
       @attr_writers << name unless @attr_writers.include? name
    end
  end

  def self.attribute_writers
    @attr_writers
  end

end

require 'test/unit'

class TextMyAttrWriter < Test::Unit::TestCase

  class ::SomeClass
    my_attr_writer :one, :two
  end

  def test_my_attr_writer
    methods = SomeClass.public_instance_methods(false)
    assert methods.include?(:one=), methods.inspect
    assert methods.include?(:two=), methods.inspect

    obj = SomeClass.new
    obj.one = 123
    assert obj.instance_variables.include?(:@one)
    assert_equal 123, obj.instance_variable_get(:@one)
  end
end

class TextMyAttrWriter < Test::Unit::TestCase
  class ::SomeClass
    my_attr_writer :one, :two
    my_attr_writer :three
  end

  def test_keeps_track
    assert_equal [:one, :two, :three], SomeClass.attribute_writers
  end
end
