class Parent
end

var = Parent
p Parent

class Child < var
end


Person = Struct.new(:name, :age)
p Person

dave = Person.new('dave', 21)
p dave
def dave.greet
  puts "Hello #{self.name}"
end
dave.greet


class Person
  def greet
    puts "Hello #{self.name}"
  end
end

class Person2 < Struct.new(:name, :age)
  def greet
    puts "Hello #{self.name}"
  end
end

Person3 = Struct.new(:name, :age) do # parametarizing consutruction
  def greet
    puts "Hello #{self.name}"
  end
end

class Container < (rand < 0.5 ? Array : Hash)
end
p Container.ancestors