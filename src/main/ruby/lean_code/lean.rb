line = readline()
price = 0

while line != "exit"
  price += {"Apples" => 100, "Bananas" => 150, "Cherries" => 75}[line.chop!]
  puts price
  line = readline()
end