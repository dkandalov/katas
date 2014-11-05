package ru.cubesolver

import org.junit.Test

class CubeSolverTest {
  private static String _ = " "
  private static String x = "x"

  @Test void "can assemble some surfaces"() {
    def allSurfaces = surface("""
          ---
          -x-
          ---
    """).combinations()

    def cube = assembleAsCube(allSurfaces.take(30))
    cube.each { key, value ->
      println("===${key}===")
      println(value)
    }

    assert cube.front == surface("""
          -x-
          -x-
          ---
    """)
    assert cube.top == surface("""
          xx-
          -x-
          ---
    """)
    assert cube.right == surface("""
          xxx
          xx-
          ---
    """)
    assert cube.bottom == surface("""
          xxx
          xxx
          ---
    """)
    assert cube.left == surface("""
          -xx
          xxx
          ---
    """)
    assert cube.back == surface("""
          ---
          -xx
          xxx
    """)
  }

  private static Map assembleAsCube(List<Surface> surfaces) {
    if (surfaces.size() < 6) throw new IllegalAccessException()
    def cube = [front: surfaces.first()]
    assembleAsCube(surfaces.tail(), cube)
  }

  private static Map assembleAsCube(List<Surface> surfaces, Map<String, Surface> cube) {
    if (surfaces.empty || cube.size() == 6) return cube

    def updated = { Map map, String key, value ->
      def newMap = map.clone()
      newMap[key] = value
      newMap
    }
    def haveOneX = { Object... points ->
      points.count { it == x } == 1
    }

    if (cube.top == null) {
      def topSurfaces = matchingRotationsOf(surfaces) {
        areConnectible(cube.front.topSide(), it.bottomSide())
      }
      return topSurfaces.findResult {
        def newCube = assembleAsCube(surfaces - it.surface, updated(cube, "top", it.rotatedSurface))
        newCube?.size() == 6 ? newCube : null
      }
    }

    if (cube.right == null) {
      def rightSurfaces = matchingRotationsOf(surfaces) {
        areConnectible(cube.front.rightSide(), it.leftSide()) &&
        areConnectible(cube.top.rightSide(), it.topSide()) &&
        haveOneX(cube.front.topRight, cube.top.bottomRight, it.topLeft)
      }
      return rightSurfaces.findResult {
        def newCube = assembleAsCube(surfaces - it.surface, updated(cube, "right", it.rotatedSurface))
        newCube?.size() == 6 ? newCube : null
      }
    }

    if (cube.bottom == null) {
      def bottomSurfaces = matchingRotationsOf(surfaces) {
        areConnectible(cube.front.bottomSide(), it.topSide()) &&
        areConnectible(cube.right.bottomSide(), it.rightSide()) &&
        haveOneX(cube.front.bottomRight, cube.right.bottomLeft, it.topRight)
      }
      return bottomSurfaces.findResult {
        def newCube = assembleAsCube(surfaces - it.surface, updated(cube, "bottom", it.rotatedSurface))
        newCube?.size() == 6 ? newCube : null
      }
    }

    if (cube.left == null) {
      def leftSurfaces = matchingRotationsOf(surfaces) {
        areConnectible(cube.front.leftSide(), it.rightSide()) &&
        areConnectible(cube.bottom.leftSide().reverse(), it.bottomSide()) &&
        areConnectible(cube.top.leftSide(), it.topSide()) &&
        haveOneX(cube.front.bottomLeft, cube.bottom.topLeft, it.bottomRight) &&
        haveOneX(cube.front.topLeft, cube.top.bottomLeft, it.topRight)
      }
      return leftSurfaces.findResult {
        def newCube = assembleAsCube(surfaces - it.surface, updated(cube, "left", it.rotatedSurface))
        newCube?.size() == 6 ? newCube : null
      }
    }

    def rotations = matchingRotationsOf(surfaces) {
      areConnectible(cube.top.topSide(), it.topSide()) &&
      areConnectible(cube.right.rightSide(), it.rightSide()) &&
      areConnectible(cube.bottom.bottomSide(), it.bottomSide()) &&
      areConnectible(cube.left.leftSide(), it.leftSide()) &&
      haveOneX(cube.top.topLeft, cube.left.topLeft, it.topLeft) &&
      haveOneX(cube.top.topRight, cube.right.topRight, it.topRight) &&
      haveOneX(cube.bottom.bottomRight, cube.right.bottomRight, it.bottomRight) &&
      haveOneX(cube.bottom.bottomLeft, cube.left.bottomLeft, it.bottomLeft)
    }
    rotations.empty ? null : updated(cube, "back", rotations.first().rotatedSurface)
  }

  private static List<Map> matchingRotationsOf(List<Surface> surfaces, Closure accepted) {
    surfaces.collectMany { Surface surface ->
      surface.rotations()
          .findAll { accepted(it) }
          .collect{ [surface: surface, rotatedSurface: it] }
    }
  }

  @Test void "all possible rotations of a surface"() {
    def rotations = surface(
            [x, x, _],
            [_, x, _],
            [_, x, _],
    ).rotations()

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

  @Test void "surface can be converted to string"() {
    def surface = surface(
            [x, x, _],
            [_, x, x],
            [_, x, _]
    )
    assert surface.toString() == """
      |xx-
      |-xx
      |-x-
    """.stripMargin("|").trim()
  }

  @Test void "surface can be constructed from string"() {
    def surface = surface("""
      xx-
      -xx
      -x-
    """)
    assert surface.toString() == """
      |xx-
      |-xx
      |-x-
    """.stripMargin("|").trim()
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
    def combinations = surface(
            [_, _, _],
            [_, x, _],
            [_, _, _]
    ).combinations()

    combinations.each {
      println("---")
      println(it)
    }
    assert combinations.size() == 160
  }


  private static boolean areConnectible(List side1, List side2) {
    [side1, side2].transpose().every{ it[0] == _ || it[1] == _ } && (side1[1] == x || side2[1] == x)
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

    def getTopRight() {
      topSide()[2]
    }

    def getBottomRight() {
      bottomSide()[2]
    }

    def getTopLeft() {
      topSide()[0]
    }

    def getBottomLeft() {
      leftSide()[2]
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

    List<Surface> rotations() {
      def rotate = { Surface aSurface ->
        def result = [aSurface]
        (0..2).each {
          aSurface = aSurface.rotateRight()
          result << aSurface
        }
        result
      }
      rotate(this) + rotate(this.horizontalFlip())
    }

    List<Surface> combinations() {
      positions().collectMany { row, column ->
        if (get(row, column) != x) {
          def updateSurface = copy().set(row, column, x)
          if (updateSurface.valid) {
            [updateSurface] + updateSurface.combinations()
          } else {
            [null]
          }
        } else {
          [null]
        }
      }.findAll{ it != null }.unique()
    }

    private positions(int startRow = 0, int startColumn = -1) {
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

    @Override String toString() {
      data.collect{
        it.collect { it == x ? "x" : "-" }.join("")
      }.join("\n")
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

  private static surface(String s) {
    def rows = s.trim().split("\n").collect {
        it.trim().chars.collect{ it == "x" ? x : _ }
      }
    new Surface(rows)
  }

  private static surface(List... rows) {
    new Surface(rows)
  }
}
