class Dog
  def initialize name
    @name = name
    @barked_counter = 0
  end

  def sit
    p @name + ": ..."
  end

  def bark
    @barked_counter = @barked_counter + 1
    p @name + ": Bark!!!! " + @barked_counter.to_s
  end
end

dog = Dog.new "dog"
pushistik = Dog.new "pushistik"


dog.bark
dog.sit

pushistik.bark
pushistik.bark

string = String.new
p string



class Friend
  @@myname = "Fred" # a class variable

  def initialize(name, sex, phone)
    @name, @sex, @phone = name, sex, phone
    # These are instance variables
  end

  def hello   # an instance method
    puts "Hi, I'm #{@name}."
  end

  def Friend.our_common_friend   # a class method
    puts "We are all friends of #{@@myname}."
  end
end
f1 = Friend.new("Susan","F","555-0123")
f2 = Friend.new("Tom","M","555-4567")

f1.hello                  # Hi, I'm Susan.
f2.hello                  # Hi, I'm Tom.