animal = Object.new

def animal.eat(food)
  puts "Thanks for the #{food}"
end

def animal.sleep(duration)
  puts "Zzzz " * duration
end

class << animal
  attr_accessor :number_of_lives
end
#def animal.number_of_lives=(count)
#  @number_of_lives = count
#end
#def animal.number_of_lives
#  @number_of_lives
#end

animal.eat("Hay")
animal.sleep(3)
animal.number_of_lives = 1
puts animal.number_of_lives

cat = animal.clone # <-- inherits singleton methods
cat.eat("Pizza")
cat.sleep(5)
cat.number_of_lives = 9
puts cat.number_of_lives

def animal.with_lives(n)
  result = self.clone
  result.number_of_lives = n
  result
end

dog = animal.with_lives(5)
puts dog.number_of_lives

def dog.sleep(duration)
  puts "Slobber " * duration
end

cat.sleep(4)
dog.sleep(3)