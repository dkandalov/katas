# odds of tossing coin twice and getting two heads
class Coin
  def self.toss
    rand(2) == 1 ? true : false
  end

  def self.toss_twice
    toss == true and toss == true
#    toss == toss # gives 0.5, 0.5
  end
end

def make attempts
  (1..attempts).collect { yield }
end

def analyze results
  head = 0
  tail = 0
  results.each { |b|
    head = head + 1 if b
    tail = tail + 1 if not b
  }
  [head.to_f/results.size, tail.to_f/results.size]
end

p analyze make(100000) { Coin.toss } # [0.5, 0.5]
p analyze make(50000) { Coin.toss_twice } # [0.25, 0.75]
# 1, 1, 0


require "set"
class Fixnum
  def factorial_recursive
    self <= 1 ? 1 : self * (self - 1).factorial
  end

  def factorial
    f = 1
    for i in 1..self
      f *= i
    end
    f
  end
end


# odds that two persons in a class have the same birthday
DAYS = 20
NUMBER_OF_PEOPLE = 2
class Person
  attr_reader :bd

  def initialize
    @bd = rand(DAYS) + 1
  end
end

def experiment number_of_people
  people = (1..number_of_people).collect { Person.new }
  set = Set.new
  people.each { |person| set << person.bd }
  set.size != people.size
end

p analyze((1..10000).collect { experiment NUMBER_OF_PEOPLE })
p (1..(NUMBER_OF_PEOPLE-1)).inject(1) {|result, i| result * ((DAYS - i).to_f/DAYS) } # or (1 - i.to_f/DAYS)
p (1..(NUMBER_OF_PEOPLE-1)).inject(1) {|result, i| result * (i.to_f/DAYS) }
p (1.0/DAYS) + (2.0/DAYS)*((4.0/DAYS)) + (3.0/DAYS)*((4.0/DAYS) * (3.0/DAYS)) + (4.0/DAYS)*((4.0/DAYS) * (3.0/DAYS) * (2.0/DAYS))


