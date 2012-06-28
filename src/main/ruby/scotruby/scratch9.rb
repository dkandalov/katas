def shout(str)
  puts str.upcase
end
shout("hello")

def shout2(str)
  puts str.to_s.upcase
end

shout2(1)
shout2(/asfsa/)
shout2(String)
shout2(nil)
shout2(method(:shout))


######################################


def distance(*args)
  @known_answers ||= {}
  @known_answers[args] ||= expensive_distance_calculation(*args)
end

def expensive_distance_calculation(x, y, z)
  # ...
end