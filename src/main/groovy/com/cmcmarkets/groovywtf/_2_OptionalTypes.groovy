package com.cmcmarkets.groovywtf

List<Integer> methodWithTypes(int n) {
  return [n]
}
def methodWithoutTypes(n) {
  return [n]
}
assert methodWithTypes(123) == methodWithoutTypes(123)

methodWithoutTypes("abc") // works
methodWithTypes("abc") // will throw an exception




