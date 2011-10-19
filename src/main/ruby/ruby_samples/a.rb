def zzz
  print "zzz"
end

a = 1 + 2
a = %q[As Magritte said, "Ceci n'est pas une pipe."]
a = "Hello world"

#p "Nadia".length
#zzz
#p "dima".upcase


str = <<EOF
Nadia
Nadia2
EOF
str.each do |line|
  p line + "!!!"
end

[1,5,7].each do |i|
  p i
end

if "a" > "z"
  p "Mr. Green"
else
  p "Beer"
end

s1 = "It was a dark and stormy night."
words = s1.split(" ")
p words
words.each do |word|
  p word
end

s2 = "apples, pears, and peaches"
list = s2.split("a")
p list


str = "Humpty Dumpty"
p str[5..7]
