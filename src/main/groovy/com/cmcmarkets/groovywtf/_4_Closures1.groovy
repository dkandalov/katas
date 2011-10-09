package com.cmcmarkets.groovywtf

def addNumbers = { int a, int b -> a + b } // kind of function literal
assert addNumbers(1, 2) == 3

// method that takes "function" as a parameter
def addToClosure(int n, Closure plus) {
  n + plus(n, 2)
}

assert addToClosure(1, addNumbers) == 4
assert addToClosure(1, { a, b -> a + b }) == 4
assert addToClosure(1) { a, b -> a + b } == 4

def i = 123
assert addToClosure(1) { a, b -> i } == 124



