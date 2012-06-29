
#################################

class X
  def do_something
  end
end

x = X.new

class X
  def something_else
  end
end

y = X.new

y.do_something
y.something_else
x.something_else


#################################


animal = "cat"

class  << animal
  def speak
    puts "miaow"
  end
end
# OR
#def animal.speak
#  puts "miaow"
#end

animal.speak

#################################

puts 1
p self
a = class Dave
  #puts 2
  #99
  p self
  self
end
puts 3
puts a

cls = Class.new do
  p self
end
p cls
Dave = cls # clever stuff that changes anonymous class name... this is the same as a normal class definition
p cls

