package com.cmcmarkets.groovywtf

// Java
List list = new ArrayList();
list.add(1);
list.add(2);
list.add(3);
// Groovy
assert [1, 2, 3] == list

// Java
Map map = new HashMap();
map.put(1, "a");
map.put(2, "b");
map.put(3, "c");
// Groovy
assert [1: "a", 2: "b", 3: "c"] == map






