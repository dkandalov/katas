package ru.cubesolver

import org.junit.Test

class CubeSolverTest {
  private static String _ = " "
  private static String x = "x"

  @Test void "aaa"() {
    def allSurfaces = combinationsOf(surface(
            [_, _, _],
            [_, x, _],
            [_, _, _]
    ))
    def map = connect(allSurfaces.first(), allSurfaces.tail().take(30))
    println(allSurfaces.first())
    map.each { key, value ->
      println("----")
      println(key)
      println(value)
    }
  }

  private static List<Map> matchingRotationsOf(List<Surface> surfaces, Closure accepted) {
    surfaces.collectMany { Surface surface ->
      rotationsOf(surface)
              .findAll { accepted(it) }
              .collect{ [surface: surface, rotatedSurface: it] }
    }
  }

  private static Map connect(Surface centerSurface, List<Surface> surfaces, Map<String, Surface> result = [:]) {
    if (surfaces.empty || result.size() == 5) return result

    if (result.top == null) {
      def topSurfaces = matchingRotationsOf(surfaces) {
        areConnectible(centerSurface.topSide(), it.bottomSide())
      }
      if (topSurfaces.empty) return result
      result.top = topSurfaces.find {
        Map newResult = result.clone() as Map
        newResult.top = it.rotatedSurface
        Map subResult = connect(centerSurface, surfaces - it.surface, newResult)
        subResult.right != null
      }?.rotatedSurface
      if (result.top == null) return result
      surfaces -= result.top
    }

    if (result.right == null) {
      def rightSurfaces = matchingRotationsOf(surfaces) {
        areConnectible(centerSurface.rightSide(), it.leftSide()) &&
        areConnectible(result.top.rightSide(), it.topSide()) &&
        [centerSurface.topSide()[2], result.top.bottomSide()[2], it.leftSide()[0]].count{it == x} == 1
      }
      if (rightSurfaces.empty) return result
      result.right = rightSurfaces.find {
        Map newResult = result.clone() as Map
        newResult.right = it.rotatedSurface
        Map subResult = connect(centerSurface, surfaces - it.surface, newResult)
        subResult.bottom != null
      }?.rotatedSurface
      if (result.right == null) return result
      surfaces -= result.right
    }

    if (result.bottom == null) {
      def bottomSurfaces = matchingRotationsOf(surfaces) {
        areConnectible(centerSurface.bottomSide(), it.topSide()) &&
        areConnectible(result.right.bottomSide(), it.rightSide()) &&
        [centerSurface.bottomSide()[2], result.right.leftSide()[2], it.topSide()[2]].count{it == x} == 1
      }
      if (bottomSurfaces.empty) return result
      result.bottom = bottomSurfaces.find {
        Map newResult = result.clone() as Map
        newResult.bottom = it.rotatedSurface
        Map subResult = connect(centerSurface, surfaces - it.surface, newResult)
        subResult.left != null
      }?.rotatedSurface
      if (result.bottom == null) return result
      surfaces -= result.bottom
    }

    if (result.left == null) {
      def leftSurfaces = matchingRotationsOf(surfaces) {
        areConnectible(centerSurface.leftSide(), it.rightSide()) &&
        areConnectible(result.bottom.leftSide().reverse(), it.bottomSide()) &&
        areConnectible(result.top.leftSide(), it.topSide()) &&
        [centerSurface.leftSide()[2], result.bottom.topSide()[0], it.rightSide()[2]].count{it == x} == 1 &&
        [centerSurface.leftSide()[0], result.top.bottomSide()[0], it.rightSide()[0]].count{it == x} == 1
      }
      if (leftSurfaces.empty) return result
      result.left = leftSurfaces.find {
        Map newResult = result.clone() as Map
        newResult.left = it.rotatedSurface
        Map subResult = connect(centerSurface, surfaces - it.surface, newResult)
        subResult.back != null
      }?.rotatedSurface
      if (result.left == null) return result
      surfaces -= result.left
    }


    def rotations = matchingRotationsOf(surfaces) {
      areConnectible(result.top.topSide(), it.topSide()) &&
              areConnectible(result.right.rightSide(), it.rightSide()) &&
              areConnectible(result.bottom.bottomSide(), it.bottomSide()) &&
              areConnectible(result.left.leftSide(), it.leftSide()) &&
              [result.top.leftSide()[0], result.left.topSide()[0], it.topSide()[0]].count { it == x } == 1 &&
              [result.top.rightSide()[0], result.right.topSide()[2], it.topSide()[2]].count { it == x } == 1 &&
              [result.bottom.rightSide()[2], result.right.bottomSide()[2], it.bottomSide()[2]].count { it == x } == 1 &&
              [result.bottom.leftSide()[2], result.left.bottomSide()[0], it.bottomSide()[0]].count { it == x } == 1
    }
    result.back = rotations.empty ? null : rotations.first().rotatedSurface


    result
  }

  @Test void "all possible rotations of a surface"() {
    def rotations = rotationsOf(surface(
            [x, x, _],
            [_, x, _],
            [_, x, _],
    ))
    assert rotations.size() == 8
    assert rotations[4] == surface(
            [_, x, x],
            [_, x, _],
            [_, x, _],
    )
    assert rotations[7] == surface(
            [x, _, _],
            [x, x, x],
            [_, _, _],
    )
  }

  private static List<Surface> rotationsOf(Surface surface) {
    def rotate = { Surface aSurface ->
      def result = [aSurface]
      (0..2).each {
        aSurface = aSurface.rotateRight()
        result << aSurface
      }
      result
    }
    rotate(surface) + rotate(surface.horizontalFlip())
  }

  @Test void "determine if surface is valid"() {
    assert !isValid(surface(
          [_, _, _],
          [_, _, _],
          [_, _, _],
    ))
    assert !isValid(surface(
          [x, _, _],
          [_, x, _],
          [_, _, _],
    ))
    assert isValid(surface(
          [x, x, _],
          [_, x, _],
          [_, _, _],
    ))
  }

  @Test void "surfaces can be rotated"() {
    assert surface(
            [x, x, _],
            [_, x, _],
            [_, x, _],
    ).rotateRight() == surface(
            [_, _, x],
            [x, x, x],
            [_, _, _],
    )
    assert surface(
            [_, _, x],
            [x, x, x],
            [_, _, _],
    ).rotateRight() == surface(
            [_, x, _],
            [_, x, _],
            [_, x, x],
    )
  }

  @Test void "surfaces can be horizontally flipped"() {
    assert surface(
            [x, x, _],
            [_, x, _],
            [_, x, _],
    ).horizontalFlip() == surface(
            [_, x, x],
            [_, x, _],
            [_, x, _],
    )
  }

  @Test void "sides of surface"() {
    def surface = surface(
            [x, x, _],
            [_, x, x],
            [_, x, x]
    )
    assert surface.topSide() == [x, x, _]
    assert surface.rightSide() == [_, x, x]
    assert surface.bottomSide() == [_, x, x]
    assert surface.leftSide() == [x, _, _]
  }

  @Test void "sides can be connectible"() {
    assert !areConnectible([_, _, x], [x, x, x])
    assert !areConnectible([_, _, x], [x, _, _])
    assert areConnectible([_, x, x], [_, _, _])
    assert areConnectible([_, _, x], [x, x, _])
  }

  @Test void "all possible valid combinations of surfaces"() {
    def allSurfaces = combinationsOf(surface(
            [_, _, _],
            [_, x, _],
            [_, _, _]
    ))
    allSurfaces.each {
      println("---")
      println(it)
    }
    assert allSurfaces.size() == 160
  }

  private static boolean areConnectible(List side1, List side2) {
    [side1, side2].transpose().every{ it[0] == _ || it[1] == _ } && (side1[1] == x || side2[1] == x)
  }

  private static List<Surface> combinationsOf(Surface surface) {
    positions().collectMany { row, column ->
      if (surface.get(row, column) != x) {
        def updateSurface = surface.copy().set(row, column, x)
        if (updateSurface.valid) {
          [updateSurface] + combinationsOf(updateSurface)
        } else {
          [null]
        }
      } else {
        [null]
      }
    }.findAll{ it != null }.unique()
  }

  private static positions(int startRow = 0, int startColumn = -1) {
    int row = startRow
    int column = startColumn
    new Iterator() {
      @Override Object next() {
        column++
        if (column >= 3) {
          row++
          column = 0
        }
        [row, column]
      }

      @Override boolean hasNext() {
        row != 2 || column != 2
      }
    }
  }


  private static class Surface {
    final List<List> data

    Surface(List<List> data) {
      this.data = data
    }

    Surface(List... rows) {
      this.data = rows.toList()
    }

    def get(int row, int column) {
      data[row][column]
    }

    def set(int row, int column, value) {
      data[row][column] = value
      this
    }

    List topSide() {
      data[0]
    }

    List bottomSide() {
      data[2]
    }

    List rightSide() {
      (0..2).collect{ data[it][2] }
    }

    List leftSide() {
      (0..2).collect{ data[it][0] }
    }

    Surface copy() {
      data.collect{ it.clone() }.toList()
    }

    boolean isValid() {
      if (data[1][1] != x) false
      else if (data[0][0] == x && data[0][1] != x && data[1][0] != x) false
      else if (data[0][2] == x && data[0][1] != x && data[1][2] != x) false
      else if (data[2][2] == x && data[1][2] != x && data[2][1] != x) false
      else if (data[2][0] == x && data[2][1] != x && data[1][0] != x) false
      else true
    }

    Surface rotateRight() {
      def newData =
        (0..2).collect { column ->
          (2..0).collect { row ->
            data[row][column]
          }
        }
      new Surface(newData)
    }

    Surface horizontalFlip() {
      def newData =
        (0..2).collect { row ->
          (2..0).collect { column ->
            data[row][column]
          }
        }
      new Surface(newData)
    }

    @Override String toString() {
      data.collect{it.join("")}.join("\n")
    }

    @Override boolean equals(o) {
      if (this.is(o)) return true
      if (getClass() != o.class) return false

      Surface surface = (Surface) o

      if (data != surface.data) return false

      return true
    }

    @Override int hashCode() {
      return (data != null ? data.hashCode() : 0)
    }
  }

  private static boolean isValid(Surface surface) {
    surface.valid
  }

  private static surface(List... rows) {
    new Surface(rows)
  }
}
