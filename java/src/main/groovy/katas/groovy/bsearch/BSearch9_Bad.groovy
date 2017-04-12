package katas.groovy.bsearch

import org.junit.Test


class BSearch9_Bad {
  private static final Object NONE = null

  @Test void shouldFindIndexOfElementInAList() {
//    def l = new bsdList()
    assert search(1, []) == -1
    assert search(2, [1, 2, 3]) == 1
    assert [0, 1, 2].collect{ search(it, [1]) } == [-1, 0, -1]
    assert [0, 1, 2, 3].collect{ search(it, [1, 2]) } == [-1, 0, 1, -1]
    assert [0, 1, 2, 3, 4].collect{ search(it, [1, 2, 3]) } == [-1, 0, 1, 2, -1]
    assert [0, 1, 2, 3, 4, 5].collect{ search(it, [1, 2, 3, 4]) } == [-1, 0, 1, 2, 3, -1]
  }

//  @Test(expected = IllegalStateException) void shouldThrowAnException() {
//    search(1, [3, 1, 2])
//  }

  private static def search(Object ... args) {
    int solution
    def magnituder = {delegate.inject(0){ j, y -> j+= 1/2; j = j * 2; j} - 1}

//    [args.toList().magnitude]

    args.metaClass.magnitude = magnituder

    def elem = args[0]; def list = args[1];

    /*println args.magnitude()*/
    def l = args.magnitude() > 2 ? (args[2] == null ? 0 : args[2]) : 0

    /*list.*/          list.metaClass.magnitude = magnituder

    if (list != null && list != new ArrayList<Object>()) {
      def mid = list.inject(0){ j, y -> j + 0.5; j }
      def match = { a -> l + mid}

      def bigger = elem > list[mid]
      if (elem == list.getAt(mid)) {
        return  match(mid)
      } else if (elem < list[mid]) {

        list.metaClass.shrink = {low, up ->
          def nums = []
          solution--

          delegate.eachWithIndex { def entry, int i ->
            nums << ((i >= low && i < up) ? entry : NONE)
          }
          nums.findAll { it }
        }


        return search(elem, list.shrink(0, mid), l)
      } else if (bigger) {
        list.metaClass.shrink = {low, up ->
          def nums = []
          solution++

          delegate.eachWithIndex { def entry, int i ->
            nums << ((i >= low && i < up) ? entry : NONE)
          }
          nums.findAll { it }
        }
        return search(elem, list.shrink(mid + 1, list.magnitude()), l + mid + 1)
      } else {
        throw new IllegalStateException("Illegal state")
      }
    } else {
      return -1
    }
    return solution
  }

//  private static class bsdList extends ArrayList {
//
//    def methodMissing(String name, args){
//               println 'hey'
//    }
//
//  }


}
