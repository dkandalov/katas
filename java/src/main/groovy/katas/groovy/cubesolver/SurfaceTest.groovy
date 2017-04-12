package katas.groovy.cubesolver
import org.junit.Test

import static katas.groovy.cubesolver.Surface.*

class SurfaceTest {

  @Test void "surface has sides"() {
    def surface = surface(
            [Surface.x, Surface.x, Surface._],
            [Surface._, Surface.x, Surface.x],
            [Surface._, Surface.x, Surface.x]
    )
    assert surface.topSide() == [Surface.x, Surface.x, Surface._]
    assert surface.rightSide() == [Surface._, Surface.x, Surface.x]
    assert surface.bottomSide() == [Surface._, Surface.x, Surface.x]
    assert surface.leftSide() == [Surface.x, Surface._, Surface._]
  }

  @Test void "surface sides can be connected"() {
    assert !areConnectible([Surface._, Surface._, Surface.x], [Surface.x, Surface.x, Surface.x])
    assert !areConnectible([Surface._, Surface._, Surface.x], [Surface.x, Surface._, Surface._])
    assert areConnectible([Surface._, Surface.x, Surface.x], [Surface._, Surface._, Surface._])
    assert areConnectible([Surface._, Surface._, Surface.x], [Surface.x, Surface.x, Surface._])
  }

  @Test void "surface can be converted to string"() {
    def surface = surface(
            [Surface.x, Surface.x, Surface._],
            [Surface._, Surface.x, Surface.x],
            [Surface._, Surface.x, Surface._]
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

  @Test void "surfaces have size"() {
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

  @Test void "surface can be rotated"() {
    assert surface(
            [Surface.x, Surface.x, Surface._],
            [Surface._, Surface.x, Surface._],
            [Surface._, Surface.x, Surface._],
    ).rotateRight() == surface(
            [Surface._, Surface._, Surface.x],
            [Surface.x, Surface.x, Surface.x],
            [Surface._, Surface._, Surface._],
    )
    assert surface(
            [Surface._, Surface._, Surface.x],
            [Surface.x, Surface.x, Surface.x],
            [Surface._, Surface._, Surface._],
    ).rotateRight() == surface(
            [Surface._, Surface.x, Surface._],
            [Surface._, Surface.x, Surface._],
            [Surface._, Surface.x, Surface.x],
    )
  }

  @Test void "surface can be horizontally flipped"() {
    assert surface(
            [Surface.x, Surface.x, Surface._],
            [Surface._, Surface.x, Surface._],
            [Surface._, Surface.x, Surface._],
    ).horizontalFlip() == surface(
            [Surface._, Surface.x, Surface.x],
            [Surface._, Surface.x, Surface._],
            [Surface._, Surface.x, Surface._],
    )
  }

  @Test void "all possible rotations of a surface"() {
    def rotations = surface(
            [Surface.x, Surface.x, Surface._],
            [Surface._, Surface.x, Surface._],
            [Surface._, Surface.x, Surface._],
    ).rotations()

    assert rotations.size() == 8
    assert rotations[4] == surface(
            [Surface._, Surface.x, Surface.x],
            [Surface._, Surface.x, Surface._],
            [Surface._, Surface.x, Surface._],
    )
    assert rotations[7] == surface(
            [Surface.x, Surface._, Surface._],
            [Surface.x, Surface.x, Surface.x],
            [Surface._, Surface._, Surface._],
    )
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

}
