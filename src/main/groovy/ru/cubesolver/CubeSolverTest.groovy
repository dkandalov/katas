package ru.cubesolver

import org.junit.Test

class CubeSolverTest {
  private static String _ = " "
  private static String x = "x"

  @Test void "surfaces can have different size"() {
    assert surface("""
          ---
          -x-
          ---
    """).size() == 3
    assert surface("""
          -----
          -xx--
          -xx--
          -----
          -----
    """).size() == 5
  }

  @Test void "can assemble some surfaces"() {
    def allSurfaces = surface("""
          ---
          -x-
          ---
    """).combinations()

    def cube = assembleAsCube(allSurfaces.toList().take(30))
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

  @Test void "all possible valid combinations of 3x3 surfaces"() {
    def combinations = surface("""
        ---
        -x-
        ---
    """).combinations()

    combinations.each {
      println("=====")
      println(it)
    }
    assert combinations.size() == 160
  }

  @Test void "all possible valid combinations of 4x4 surfaces"() {
    def combinations = surface("""
        ----
        -xx-
        -xx-
        ----
    """).combinations()

    def size = combinations.size()
    assert size == 2400
  }

  @Test void "all possible valid combinations of 5x5 surfaces"() {
    def combinations = surface("""
        -----
        -xxx-
        -xxx-
        -xxx-
        -----
    """).combinations()

    def size = combinations.size()
    assert size == 38415
  }

  private static boolean areConnectible(List side1, List side2) {
    int centerIndex1 = side1.size().intdiv(2)
    int centerIndex2 = (side1.size() - 1).intdiv(2)
    [side1, side2].transpose().every { it[0] == _ || it[1] == _ } &&
      (side1[centerIndex1] == x || side2[centerIndex1] == x) &&
      (side1[centerIndex2] == x || side2[centerIndex2] == x)
  }


  private static class Surface {
    final List<List> data
    final int lastIndex

    Surface(List... rows) {
      this(rows.toList())
    }

    Surface(List<List> data) {
      this.data = data
      this.lastIndex = data[0].size() - 1
    }

    List topSide() {
      data[0]
    }

    List bottomSide() {
      data[lastIndex]
    }

    List rightSide() {
      (0..lastIndex).collect{ data[it][lastIndex] }
    }

    List leftSide() {
      (0..lastIndex).collect{ data[it][0] }
    }

    def getTopRight() {
      topSide()[lastIndex]
    }

    def getBottomRight() {
      bottomSide()[lastIndex]
    }

    def getTopLeft() {
      topSide()[0]
    }

    def getBottomLeft() {
      bottomSide()[0]
    }

    int size() {
      lastIndex + 1
    }

    Surface rotateRight() {
      def newData =
        (0..lastIndex).collect { column ->
          (lastIndex..0).collect { row ->
            data[row][column]
          }
        }
      new Surface(newData)
    }

    Surface horizontalFlip() {
      def newData =
        (0..lastIndex).collect { row ->
          (lastIndex..0).collect { column ->
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

    LinkedHashSet<Surface> combinations(LinkedHashSet<Surface> result = []) {
      positions().each { row, column ->
        if (isValidUpdate(row, column)) {
          def updateSurface = copy()
          updateSurface.data[row][column] = x
          if (!result.contains(updateSurface)) {
            result.add(updateSurface)
            updateSurface.combinations(result)
          }
        }
      }
      result
    }

    private boolean isValidUpdate(int row, int column) {
      if (row == 0 && column == 0) topSide()[1] == x || leftSide()[1] == x
      else if (row == 0 && column == lastIndex) topSide()[lastIndex - 1] == x || rightSide()[1] == x
      else if (row == lastIndex && column == lastIndex) bottomSide()[lastIndex - 1] == x || rightSide()[lastIndex - 1] == x
      else if (row == lastIndex && column == 0) bottomSide()[1] == x || leftSide()[lastIndex - 1] == x
      else data[row][column] != x
    }

    private Surface copy() {
      new Surface(data.collect{ it.clone() }.toList())
    }

    private positions(int row = 0, int column = -1) {
      new Iterator() {
        @Override Object next() {
          column++
          if (column > lastIndex) {
            row++
            column = 0
          }
          [row, column]
        }

        @Override boolean hasNext() {
          row != lastIndex || column != lastIndex
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
