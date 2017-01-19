animal = "cat"
class << animal
  def speak
  end
end
animal.speak

class Person
  @fred = 12

  class << self
    attr_accessor :yyy

    def xxx
    end

    def fred
      @fred
    end

    def fred=(val)
      @fred = val
    end
  end

  def getFred
    @fred
  end
end

Person.xxx
f = Person.new
p f.getFred # nil

p Person.fred
Person.fred = 99
p Person.fred


#################################


class Fixnum
  alias :old_plus :+

  def +(other)
    self.old_plus(other) * 3
  end
end

p "1 + 2 = #{1 + 2}"