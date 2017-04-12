package katas.groovy.cubesolver
import static katas.groovy.cubesolver.Surface.*

class CubeAssembler {
  static Map assembleAsCube(Collection<Surface> surfaces) {
    if (surfaces.size() < 6) throw new IllegalArgumentException()
    surfaces = surfaces.toList()

    def cube = [front: surfaces.first()]
    doAssembleAsCube(surfaces.tail(), cube)
  }

  private static Map doAssembleAsCube(List<Surface> surfaces, Map<String, Surface> cube) {
    if (surfaces.empty || cube.size() == 6) return cube

    def updated = { Map map, String key, value ->
      def newMap = map.clone()
      newMap[key] = value
      newMap
    }
    def haveOneX = { Object... points ->
      points.count { it == Surface.x } == 1
    }

    if (cube.top == null) {
      def topSurfaces = matchingRotationsOf(surfaces) {
        areConnectible(cube.front.topSide(), it.bottomSide())
      }
      return topSurfaces.findResult {
        def newCube = doAssembleAsCube(surfaces - it.surface, updated(cube, "top", it.rotatedSurface))
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
        def newCube = doAssembleAsCube(surfaces - it.surface, updated(cube, "right", it.rotatedSurface))
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
        def newCube = doAssembleAsCube(surfaces - it.surface, updated(cube, "bottom", it.rotatedSurface))
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
        def newCube = doAssembleAsCube(surfaces - it.surface, updated(cube, "left", it.rotatedSurface))
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
}
