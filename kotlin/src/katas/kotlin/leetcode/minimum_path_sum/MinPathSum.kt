package katas.kotlin.leetcode.minimum_path_sum

import datsok.shouldEqual
import org.junit.jupiter.api.Test

//
// Given a m x n grid filled with non-negative numbers, find a path from top left to bottom right,
// which minimizes the sum of all numbers along its path.
// Note: You can only move either down or right at any point in time.
//
// m == grid.length
// n == grid[i].length
// 1 <= m, n <= 200
// 0 <= grid[i][j] <= 100
//
// https://leetcode.com/problems/minimum-path-sum ✅
//
class MinPathSum {
    @Test fun `Example 1`() {
        val grid = arrayOf(
            intArrayOf(1, 3, 1),
            intArrayOf(1, 5, 1),
            intArrayOf(4, 2, 1)
        )
        findMinSumPath(grid) shouldEqual listOf(1, 3, 1, 1, 1)
        minPathSum(grid) shouldEqual 7
    }

    @Test fun `Example 2`() {
        val grid = arrayOf(
            intArrayOf(1, 2, 3),
            intArrayOf(4, 5, 6)
        )
        findMinSumPath(grid) shouldEqual listOf(1, 2, 3, 6)
        minPathSum(grid) shouldEqual 12
    }

    @Test fun `empty grid`() {
        findMinSumPath(grid = arrayOf()) shouldEqual emptyList()
    }

    @Test fun `single cell grid`() {
        findMinSumPath(grid = arrayOf(intArrayOf(1))) shouldEqual listOf(1)
    }

    @Test fun `one row grid`() {
        findMinSumPath(grid = arrayOf(
            intArrayOf(1, 2, 3)
        )) shouldEqual listOf(1, 2, 3)
    }

    @Test fun `one column grid`() {
        findMinSumPath(grid = arrayOf(
            intArrayOf(1),
            intArrayOf(2),
            intArrayOf(3),
        )) shouldEqual listOf(1, 2, 3)
    }

    @Test fun `200x200 grid`() {
        findMinSumPath(grid = Array(size = 200) {
            IntArray(200) { 1 }
        }) shouldEqual List(200 + 199) { 1 }
    }

    private fun minPathSum(grid: Array<IntArray>): Int = findMinSumPath(grid).sum()

    private fun findMinSumPath(
        grid: Array<IntArray>,
        x: Int = 0,
        y: Int = 0,
        cache: HashMap<Pair<Int, Int>, List<Int>> = HashMap()
    ): List<Int> {
        if (grid.isEmpty()) return emptyList()
        if (cache[Pair(x, y)] != null) return cache[Pair(x, y)]!!

        val maxX = grid.first().size - 1
        val maxY = grid.size - 1
        if (x == maxX && y == maxY) return listOf(grid[y][x])

        val pathRight = if (x + 1 <= maxX) findMinSumPath(grid, x + 1, y, cache) else null
        val pathDown = if (y + 1 <= maxY) findMinSumPath(grid, x, y + 1, cache) else null

        val minPath = listOf(grid[y][x]) + listOfNotNull(pathRight, pathDown).minBy { it.sum() }
        cache[Pair(x, y)] = minPath
        return minPath
    }
}