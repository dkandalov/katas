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