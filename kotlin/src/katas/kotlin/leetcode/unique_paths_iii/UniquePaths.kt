package katas.kotlin.leetcode.unique_paths_iii

import datsok.shouldEqual
import org.junit.jupiter.api.Test

// https://leetcode.com/problems/unique-paths-iii ✅
//
// On a 2-dimensional grid, there are 4 types of squares:
//   1 represents the starting square. There is exactly one starting square.
//   2 represents the ending square. There is exactly one ending square.
//   0 represents empty squares we can walk over.
//   -1 represents obstacles that we cannot walk over.
// Return the number of 4-directional walks from the starting square to the ending square,
// that walk over every non-obstacle square exactly once.
//
// Example 1:
// Input:
//   [[1,0,0,0],
//    [0,0,0,0],
//    [0,0,2,-1]]
// Output: 2
// Explanation: We have the following two paths:
// 1. (0,0),(0,1),(0,2),(0,3),(1,3),(1,2),(1,1),(1,0),(2,0),(2,1),(2,2)
// 2. (0,0),(1,0),(2,0),(2,1),(1,1),(0,1),(0,2),(0,3),(1,3),(1,2),(2,2)
//
// Example 2:
// Input:
//   [[1,0,0,0],
//    [0,0,0,0],
//    [0,0,0,2]]
// Output: 4
// Explanation: We have the following four paths:
// 1. (0,0),(0,1),(0,2),(0,3),(1,3),(1,2),(1,1),(1,0),(2,0),(2,1),(2,2),(2,3)
// 2. (0,0),(0,1),(1,1),(1,0),(2,0),(2,1),(2,2),(1,2),(0,2),(0,3),(1,3),(2,3)
// 3. (0,0),(1,0),(2,0),(2,1),(2,2),(1,2),(1,1),(0,1),(0,2),(0,3),(1,3),(2,3)
// 4. (0,0),(1,0),(2,0),(2,1),(1,1),(0,1),(0,2),(0,3),(1,3),(1,2),(2,2),(2,3)
//
// Example 3:
// Input: [[0,1],[2,0]]
// Output: 0
// Explanation:
// There is no path that walks over every empty square exactly once.
// Note that the starting and ending square can be anywhere in the grid.


class Tests {
    @Test fun examples() {
        findAllUniquePaths(grid = arrayOf(
            intArrayOf(1,0,0,0),
            intArrayOf(0,0,0,0),
            intArrayOf(0,0,2,-1)
        )).toString() shouldEqual "[" +
            "[(0, 0), (0, 1), (0, 2), (0, 3), (1, 3), (1, 2), (1, 1), (1, 0), (2, 0), (2, 1), (2, 2)], " +
            "[(0, 0), (1, 0), (2, 0), (2, 1), (1, 1), (0, 1), (0, 2), (0, 3), (1, 3), (1, 2), (2, 2)]" +
        "]"

        findAllUniquePaths(grid = arrayOf(
            intArrayOf(1,0,0,0),
            intArrayOf(0,0,0,0),
            intArrayOf(0,0,0,2)
        )).toString() shouldEqual "[" +
            "[(0, 0), (0, 1), (0, 2), (0, 3), (1, 3), (1, 2), (1, 1), (1, 0), (2, 0), (2, 1), (2, 2), (2, 3)], " +
            "[(0, 0), (0, 1), (1, 1), (1, 0), (2, 0), (2, 1), (2, 2), (1, 2), (0, 2), (0, 3), (1, 3), (2, 3)], " +
            "[(0, 0), (1, 0), (2, 0), (2, 1), (2, 2), (1, 2), (1, 1), (0, 1), (0, 2), (0, 3), (1, 3), (2, 3)], " +
            "[(0, 0), (1, 0), (2, 0), (2, 1), (1, 1), (0, 1), (0, 2), (0, 3), (1, 3), (1, 2), (2, 2), (2, 3)]" +
        "]"

        findAllUniquePaths(grid = arrayOf(
            intArrayOf(1,0),
            intArrayOf(0,2)
        )) shouldEqual emptyList()
    }
}

fun uniquePaths(grid: Array<IntArray>): Int {
    return findAllUniquePaths(grid).size
}

private fun findAllUniquePaths(grid: Grid): List<Path> {
    val startPoint = grid.points().find { grid[it] == 1 }!!
    val endPoint = grid.points().find { grid[it] == 2 }!!
    val expectedSize = grid.points().count { grid[it] != -1 }

    return grid.allPaths(startPoint, endPoint).filter { it.size == expectedSize }
}

private fun Grid.allPaths(startPoint: Point, endPoint: Point, path: Path = listOf(startPoint)): List<Path> {
    if (startPoint == endPoint) return listOf(path)

    val nextPoints = nextMoves(startPoint) - path
    if (nextPoints.isEmpty()) return emptyList()

    return nextPoints.flatMap { point ->
        allPaths(point, endPoint, path + point)
    }
}

private operator fun Grid.get(point: Point): Int =
    this[point.first][point.second]

private fun Grid.nextMoves(point: Point): List<Point> {
    val (y, x) = point
    return listOfNotNull(
        if (x - 1 in this[y].indices) Point(y, x - 1) else null,
        if (x + 1 in this[y].indices) Point(y, x + 1) else null,
        if (y - 1 in indices) Point(y - 1, x) else null,
        if (y + 1 in indices) Point(y + 1, x) else null
    ).filter { this[it] != -1 }
}

private fun Array<IntArray>.points(): Sequence<Point> = sequence {
    this@points.forEachIndexed { y, row ->
        row.indices.onEach { x ->
            yield(Point(y, x))
        }
    }
}

typealias Grid = Array<IntArray>
typealias Point = Pair<Int, Int>
typealias Path = List<Point>
