package katas.groovy.sort.index

import org.junit.Test

/**
 * User: dima
 * Date: 18/09/2012
 */
class SymbolTable0 {
  @Test void aaa() {
    String text = """
12.5 Index Implementations with Symbol Tables
For many applications we want a search structure simply to help us find items, without moving them around.
For example, we might have an array of items with keys, and we might want the search method to give us the
index into that array of the item matching a certain key. Or we might want to remove the item with a given
index from the search structure but still keep it in the array for some other use. An important feature of this
approach is that it allows the client to maintain extra arrays (extra information associated with each node)
to be added without the symbol-table code being changed at all. When the search routine returns the index for an item,
it gives a way to access immediately all the information associated with that item, by using the index
to access an appropriate array.
"""
    // TODO
  }
}
