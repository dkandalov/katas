module Mod
end
class Cls
  include Mod
end
class Parent
end
class Child < Parent
  include Mod
end
p Cls.ancestors
p Child.ancestors


animal = "cat"
module Vocal
  def speak
    puts "miaow"
  end
  def upcase
    "XXX"
  end
end
animal.extend Vocal
animal.speak
puts animal.upcase

p animal.class.ancestors



module Vocal
  def speak
    "greetings"
  end
end
class Dave
  self.extend Vocal
  # OR
  #class << self
  #  include Vocal
  #end
end
puts Dave.speak


module Persistable
  def save
  end

  def self.included(klass)
    klass.extend ClassMethods # with this we don't need to "extend" ClassMethods below
  end

  module ClassMethods
    def find
      Person.new
    end
  end
end

class Person
  include Persistable
  extend Persistable::ClassMethods
end
person = Person.find
person.save




module Vocal
  def self.included(klass)
    def klass.hello # creates separate method for EACH included module
      puts "hello"
    end
  end
end
class Dave
  include Vocal
end
Dave.hello

