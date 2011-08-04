package com.cmcmarkets.groovywtf

def text = """this is a multiline
string literal
"""

// Java
BufferedReader reader = new BufferedReader(new StringReader(text));
try {
  assert reader.readLine().equals("this is a multiline");
  assert reader.readLine().equals("string literal");
} finally {
  reader.close();
}

// Groovy
new StringReader(text).withReader {
  assert it.readLine() == "this is a multiline"
  assert it.readLine() == "string literal"
}



