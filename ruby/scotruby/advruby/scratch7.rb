module One
  class Two
  end
end
p One.constants # ["Two"]


module Mod
end
module Mod2
end
class Cls
  def greet
    puts "howdy"
  end
  include Mod
  include Mod2
end
obj = Cls.new

module Mod
  def greet
    puts "hi"
  end
end
p Cls.ancestors
obj.greet