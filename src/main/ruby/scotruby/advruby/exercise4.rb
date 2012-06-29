require 'test/unit'
require "date"

class TestEmployee < Test::Unit::TestCase
  def setup
    @june_first = Date.new(2011, 6, 1)
    @fred = Employee.new("Fred Smith", @june_first)
    @betty = Employee.new("Betty Jones", "2011-06-20")
  end

  def test_attributes
    assert_equal "Fred Smith", @fred.name
    assert_equal @june_first, @fred.start_date

    assert_equal "Betty Jones", @betty.name
    assert_equal Date.new(2011, 6, 20), @betty.start_date
  end

  def test_to_s
    assert_equal "Fred Smith joined 2011-06-01", @fred.to_s
    assert_equal "Betty Jones joined 2011-06-20", @betty.to_s
  end
end

class TestManager < Test::Unit::TestCase
  def setup
    @barney = Manager.new("Barney Patel", "2011-01-20", "washroom 1")
  end

  def test_attributes
    assert_equal "Barney Patel", @barney.name
    assert_equal Date.new(2011, 1, 20), @barney.start_date
    assert_equal "washroom 1", @barney.washroom
  end

  def test_to_s
    assert_equal "BARNEY PATEL joined 2011-01-20 and can use washroom 1",
                 @barney.to_s
  end
end

class Employee
  attr_reader :name, :start_date

  def initialize name, start_date
    @name = name
    start_date = Date.parse(start_date) if  start_date.is_a? String
    @start_date = start_date
  end

  def to_s
    "#{@name} joined #{start_date}"
  end
end

class Manager < Employee
  attr_reader :washroom

  def initialize name, start_date, washroom
    super(name, start_date)
    @washroom = washroom
  end

  def to_s
    "#{@name.upcase} joined #{start_date} and can use #{@washroom}"
  end
end