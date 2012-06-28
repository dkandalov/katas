animal = "cat"

cls = class << animal
  puts self
  def speak
    puts "miaow"
  end
  self
end
puts cls
Wibble = cls # doesn't work :(
p cls

#cls.new # doesn't work :(

class << cls
end

#################################

class Log
  if $DEBUG
    def log(msg)
      puts "log: #{msg}"
    end
  else
    def log(msg)
    end
  end
end

class Log
  def more_stuff
  end
end if $DEBUG

condition = false
class Log
  def more_stuff_2
  end
end while condition


#################################


class Person
  def self.xxx
  end
  def Person.yyy
  end

  wibble = self

  def wibble.zzz
  end
end

my_class = Person
def my_class.greet
  puts "Howdy"
end

Person.greet


#################################


animal = "cat"
def (animal.class).speak
  puts "#{self} speaks"
end

String.speak