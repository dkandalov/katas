package katas.groovy.cubesolver
import org.junit.Test

import static katas.groovy.cubesolver.CubeAssembler.assembleAsCube
import static katas.groovy.cubesolver.Surface.*

class CubeAssemblerTest {

  @Test void "can assemble cube from some 3x3 surfaces"() {
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

  @Test void "can assemble cube from some 5x5 surfaces"() {
    def allSurfaces = surface("""
          -----
          -xxx-
          -xxx-
          -xxx-
          -----
    """).combinations()

    def cube = assembleAsCube(allSurfaces.toList().take(100))
    cube.each { key, value ->
      println("===${key}===")
      println(value)
    }

    assert cube.front == surface("""
       -x---
       -xxx-
       -xxx-
       -xxx-
       -----
    """)
    assert cube.top == surface("""
       -----
       -xxx-
       -xxx-
       -xxx-
       --xxx
    """)
    assert cube.right == surface("""
       --xx-
       xxxxx
       xxxxx
       xxxxx
       xxxxx
    """)
    assert cube.bottom == surface("""
       xxx--
       xxxx-
       xxxx-
       xxxx-
       xxx--
    """)
    assert cube.left == surface("""
       xxxxx
       xxxxx
       xxxxx
       xxxx-
       -----
    """)
    assert cube.back == surface("""
       -xxxx
       -xxx-
       -xxx-
       -xxx-
       -----
    """)
  }

  @Test void "can assemble cube using surfaces from test task"() {
    def surface1 = surface("""
      --x--
      -xxx-
      xxxxx
      -xxx-
      --x--
    """)
    def surface2 = surface("""
      x-x-x
      xxxxx
      -xxx-
      xxxxx
      x-x-x
    """)
    def surface3 = surface("""
      --x--
      -xxxx
      xxxx-
      -xxxx
      --x--
    """)
    def surface4 = surface("""
      -x-x-
      xxxx-
      -xxxx
      xxxx-
      xx-x-
    """)
    def surface5 = surface("""
      -x-x-
      xxxxx
      -xxx-
      xxxxx
      x-x--
    """)
    def surface6 = surface("""
      -x-x-
      -xxxx
      xxxx-
      -xxxx
      xx-xx
    """)
    def cube = assembleAsCube([surface1, surface2, surface3, surface4, surface5, surface6])
    println(cube)

    assert cube.front == surface1
    assert cube.top == surface2.rotateRight()
    assert cube.right == surface4
    assert cube.bottom == surface6.horizontalFlip()
    assert cube.left == surface5.horizontalFlip()
    assert cube.back == surface3
  }
}
