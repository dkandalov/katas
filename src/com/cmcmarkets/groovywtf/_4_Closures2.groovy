package com.cmcmarkets.groovywtf

[1, 2, 3].each { n -> print "$n-" } // prints "1-2-3-"
[1, 2, 3].each { print "$it-" }     // prints "1-2-3-"

assert [1, 2, 3].collect {it * 2} == [2, 4, 6]
assert [1, 2, 3].any {it >= 2} == true
assert [1, 2, 3].every {it >= 2} == false
assert [1, 2, 3].findAll {it >= 2} == [2, 3]
assert [1, 2, 3].inject(0) { acc, value -> acc + value } == 6
assert [1, 2, 3].groupBy { it % 2 } == [1: [1, 3], 0: [2]]



